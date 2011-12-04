;;;; mf-solver.lisp

(in-package #:mf-solver)

;;; "mf-solver" goes here. Hacks and glory await!

(defvar *solution*)
(defparameter *n* 6)
(defvar *idx*)
(defvar *mutex* (sb-thread:make-mutex :name "my lock"))
(defvar *wait-queue* (sb-thread:make-waitqueue))

;;;; utils

(defun print-matrix (matrix)
  (loop
     for y from 0 below (car (array-dimensions matrix))
     do
       (format t "~&")
       (loop
	  for x from 0 below (cadr (array-dimensions matrix))
	  for cell = (aref matrix y x)
	  do (format t "~a " cell))
       (terpri)))

(defun pprint-tree (v &optional (stream *standard-output*))
  (labels ((recursive-print (node level char)
             (format stream "~Vt~A ~A~%" level char (label node))
             (when (left node)
               (recursive-print (left node) (1+ level) #\l))
             (when (right node)
               (recursive-print (right node) (1+ level) #\r))))
    (recursive-print v 0 #\R)))

(defmethod draw ((v vertex))
  (pprint-tree v))

;; counter

(defmethod inc ((c counter))
  (sb-thread:with-mutex (*mutex*)
    (incf (counter c))
    (format t "~&Counter: ~A~%" (counter c))))

(defmethod dec ((c counter))
  (sb-thread:with-mutex (*mutex*)
    (when (> (counter c) 0)
      (decf (counter c)))
    (format t "~&Counter: ~A~%" (counter c))
    (when (= (counter c) 0)
      (format t "~&Counter: notify~%")
      (sb-thread:condition-broadcast *wait-queue*))))

(defmethod release ((c counter))
  (sb-thread:with-mutex (*mutex*)
    (format t "~&Wait~%")
    (sb-thread:condition-wait *wait-queue* *mutex*)
    (format t "~&Release~%")))

;; vertex

(defmethod start ((production production))
  (sb-thread:with-mutex (*mutex*)
  (let ((p (p-type production))
	(v (vertex production)))	  
  (let ((p-type (case p
		  (p1 'root)
		  ((p2 p3) 'int)))
	(label (case p
		  (p1 't)
		  ((p2 p3) 'node))))  
  (format t "~A~%" p)
  (let ((t1 (make-instance 'vertex :left nil :right nil :parent v :label label))
	(t2 (make-instance 'vertex :left nil :right nil :parent v :label label)))
    (setf (left v) t1)
    (setf (right v) t2)
    (setf (label v) p-type))
  v))))

(defmethod start :around ((p production))
  (inc (counter p))
  (sleep (random 1))
  (call-next-method)
  (draw  (vertex p))
  (dec (counter p))
  p)

(defun make-node-1 (node)
  (format t "~A~%" 'node)
  (setf (aref (m node) 1 1)  1
	(aref (m node) 2 1)  1
	(aref (m node) 1 2)  0
	(aref (m node) 2 2) -1)
  (setf (aref (v node) 1) 0
	(aref (v node) 2) 0)
  (setf (aref (idx node) 1) *idx*
	(aref (idx node) 2) (incf *idx*))
  node)

(defun make-node-n (node)
  (format t "~A~%" 'node)
  (setf (aref (m node) 1 1) -1
	(aref (m node) 2 1)  1
	(aref (m node) 1 2)  1
	(aref (m node) 2 2) -1)
  (setf (aref (v node) 1) 0
	(aref (v node) 2) 0)
  (setf (aref (idx node) 1) *idx*
	(aref (idx node) 2) (incf *idx*))
  node)

(defun make-node-last (node)
  (format t "~A~%" 'node)
  (setf (aref (m node) 1 1) -1
	(aref (m node) 2 1)  0
	(aref (m node) 1 2)  1
	(aref (m node) 2 2) 1)
  (setf (aref (v node) 1) 0
	(aref (v node) 2) 20)
  (setf (aref (idx node) 1) *idx*
	(aref (idx node) 2) (incf *idx*))
  node)

(defun make-nodes (nodes)
  (loop
     for (node next-node) on nodes
     for first-node = node then nil
     do
       (cond
	 (first-node (make-node-1 (left (vertex node)))
		     (make-node-n (right (vertex node))))
	 (next-node  (make-node-n (left (vertex node)))
		     (make-node-n (right (vertex node))))
	 (t (make-node-n (left (vertex node)))
	    (make-node-last (right (vertex node)))))))

;;concatenate nodes

(defgeneric concat-nodes (p))

(defmethod concat-nodes((production production))
  (let* ((vertex (vertex production))
	 (matrix (m vertex))
	 (v (v vertex))
	 (idx (idx vertex)))
  (setf (aref matrix 0 0) (aref (m (left vertex)) 1 1)
	(aref matrix 0 1) (aref (m (left vertex)) 1 2)
	(aref matrix 0 2) 0
	
	(aref matrix 1 0) (aref (m (left vertex)) 2 1)
	(aref matrix 1 1) (+ (aref (m (left vertex)) 2 2)
			     (aref (m (right vertex)) 1 1))
	(aref matrix 1 2) (aref (m (right vertex)) 1 2)
		
	(aref matrix 2 0) 0
	(aref matrix 2 1) (aref (m (right vertex)) 2 1)
	(aref matrix 2 2) (aref (m (right vertex)) 2 2))
  
  (setf (aref v 0) (aref (v (left vertex)) 1)
	(aref v 1) (+ (aref (v (left vertex)) 2)
			   (aref (v (right vertex)) 1))
	(aref v 2) (aref (v (right vertex)) 2))
  (assert (= (aref (idx (left vertex)) 2)
	     (aref (idx (right vertex)) 1)))
  (setf (aref idx 0) (aref (idx (left vertex)) 1)
	(aref idx 1) (aref (idx (left vertex)) 2)
	(aref idx 2) (aref (idx (right vertex)) 2))
  vertex))

(defun gaussian-elimination(vertex)
  (let ((matrix (m vertex))
	(v (v vertex))
	(idx (idx vertex)))
  ;; 1st column :=: 2nd column
    (dotimes (row 3)
      (rotatef (aref matrix row 0) (aref matrix row 1)))
  ;; 1st row :=: 2nd row
    (dotimes (col 3)
      (rotatef (aref matrix 0 col) (aref matrix 1 col)))
    (rotatef (aref v 0) (aref v 1))
    (rotatef (aref idx 0) (aref idx 1))    
  ;; gaussian-elimination
   (let ((1st (aref matrix 0 0))
	 (2nd (aref matrix 1 0))
	 (3rd (aref matrix 2 0)))
     ;;1st-row /= 1st
     (dotimes (col 3)
       (setf (aref matrix 0 col)
	     (/ (aref matrix 0 col)
		1st)))
     (setf (aref v 0)
	   (/ (aref v 0)
	      1st))
     (setf 1st (aref matrix 0 0))
     ;;2nd-row -= 1st-row * 2nd/1st
      (dotimes (col 3)
	(decf (aref matrix 1 col)
	      (* (aref matrix 0 col)
		 (/ 2nd 1st))))		 
      (decf (aref v 1) 
	    (* (aref v 0)
	       (/ 2nd 1st)))
      
      ;;3rd-row -= 1st-row * 3rd/1st
      (dotimes (col 3)
	(decf (aref matrix 2 col)
	      (* (aref matrix 0 col)
		 (/ 3rd 1st))))
      (decf (aref v 2)
	    (* (aref v 0)
	       (/ 3rd 1st)))
      (print-matrix matrix)
      
      ;;3rd-row -= 2st-row * 3rd/2nd only for root
      (when (and (eq (label vertex)
		     'root)
		 (not (zerop (aref matrix 2 1))))
	(let ((3rd2 (aref matrix 2 1)))
	  (dotimes (col 3)
	    (decf (aref matrix 2 col)
		  (* (aref matrix 1 col)
		     (/ 3rd2
			(aref matrix 1 1)))))
	  (decf (aref v 2)
		(* (aref v 1)
		   (/ 3rd2
		      (aref matrix 1 1)))))))))

;; production

(defmethod initialize-instance :after ((p production) &rest initargs) 
  (declare (ignore initargs))
  #+nil(sb-thread:make-thread  #'start :arguments p))
  

(defun solve-vertex (vertex &optional x2 x3)
  (let ((m (m vertex))
	(v (v vertex))
	(idx (idx vertex))
	(x1 nil))
    (when x2
      (setf (aref v 1) x2
	    (aref m 1 0) 0
	    (aref m 1 1) 1
	    (aref m 1 2) 0))
    (when x3
      (setf (aref v 2) x3
	    (aref m 2 0) 0
	    (aref m 2 1) 0
	    (aref m 2 2) 1))
    (unless x2
      (setf x2 (aref v 1)))
    (unless x3
      (setf x3 (aref v 2)))
    (when (aref idx 0)
      (setf x1 (/ (- (+ (* x2 (aref m 0 1))
			(* x3 (aref m 0 2))))
		  (aref m 0 0))))
    (cond
      ((aref idx 0)
       (format t "~&u~A->~A u~A->~A u~A->~A~%" (aref idx 0) x1 (aref idx 1) x2 (aref idx 2) x3)
       (setf (aref *solution* (aref idx 0)) x1
	     (aref *solution* (aref idx 1)) x2
	     (aref *solution* (aref idx 2)) x3))
      (t (format t "~&u~A->~A u~A->~A~%" (aref idx 1) x2 (aref idx 2) x3)
	 (setf (aref *solution* (aref idx 1)) x2
	       (aref *solution* (aref idx 2)) x3)))
    (values x1 x2 x3)))

(defun make-groups (list)
  (setf list (nreverse list))
  (loop
     with i = 1
     while list
     collect 
       (loop 
	  for j below i 
	  while list collect (pop list)
	  finally (setf i (* i 2)))
       into result
     finally (return (reverse result))))

(defmacro with-threads (counter &body body)
  (let ((old-io (gensym "OLD-IO"))
	(instance (gensym "INSTANCE"))
	(list (gensym "LIST")))
    `(let ((,list
	    `(,,@(mapcar (lambda (expr)
			   `(let ((,instance ,expr)
				  (,old-io *standard-output*))
			      (sb-thread:make-thread  (lambda ()
							(let ((*standard-output* ,old-io))
							  (start ,instance))))
			      ,instance))
			 body))))
       (release ,counter)
       ,list)))


#+nil(defmacro with-threads (counter &body body)
       "without threads"
  (let ((old-io *standard-output*))
    `(let ((list
	    (list ,@(mapcar (lambda (expr)
			`(start ,expr))
		      body))))
       ;(release ,counter)
       list)))

(defun make-tree (&optional (n *n*))
  (assert (evenp n))
  (let* ((s (make-instance 'vertex :label 'S))
	 (counter (make-instance 'counter))
	 (nodes `(,(start (make-instance 'production :p-type 'p1 :vertex s :counter counter))))
	 (limit (1- n))
	 (*idx* 0)
	 )
      (loop
	 with parents  = `((,(car nodes)))
	 with i = 1
	 while (< i limit)
	 for sub-parents = (pop parents)
	 while sub-parents
	 collect
	   (loop
	      for parent in sub-parents 
	      while (< i limit)
	      append
		(progn
		  (incf i 2)
		  (with-threads counter
		    (make-instance 'production
				   :p-type 'p2
				   :vertex (left (vertex parent))
				   :counter counter)
		    (make-instance 'production
				   :p-type 'p2
				   :vertex (right (vertex parent))
				   :counter counter)))
	      into level
	      finally (progn
			(push level parents)
			(setf nodes (append level nodes)))))
      (let ((leafs (remove-if (lambda (x)
				(left (left (vertex x)))) nodes)))
	(make-nodes leafs))
  (values s nodes)))

(defun fill-tree (nodes)
  (loop
     for group in (make-groups nodes)
     do (map nil (lambda (node)
		   (print (label (vertex node)))
		   (print node)
		   (concat-nodes node)
		   (gaussian-elimination (vertex node)))
	     group)))

(defun backward-substitution (vertex)
  (labels ((%backward-substitution% (vertex x1 x2)
	     (multiple-value-bind (x2 x1 x3) (solve-vertex vertex x1 x2)
	       (when (and (left vertex)
			  (right vertex))
		 (%backward-substitution% (left vertex) x1 x2)
		 (%backward-substitution% (right vertex) x2 x3)))))
    (unless (eq (label vertex) 'root)
      (error "Function should be called on root vertex"))
    (let ((*solution* (make-array `(,(1+ *n*)))))
      (%backward-substitution% vertex nil nil)
      *solution*)))

;;;;
;;;; entry point
;;;;


(defun main(n)
  (let ((*n* n))
    (multiple-value-bind (s nodes) (make-tree *n*)
      (fill-tree nodes)
      (backward-substitution s))))


;; old



(defmacro with-productions (list &body body)
  (let ((old-io (gensym "old-io")))
    `(let 
       ,(loop
	   for i from 0 
	   for production in list collect
	     (destructuring-bind (name (production vertex counter)) production
	       `(,name (make-instance 'production :p-type ',production :vertex ,vertex :counter ,counter))))
       (let ((,old-io *standard-output*))
	 ,@(loop
	      for production in list
	      for var = (car production) collect
		`(sb-thread:make-thread  (lambda ()
					   (let ((*standard-output* ,old-io))
					     (start ,var))))))
       ,@body)))

(defun main-old(&key (a 0) (b 1) (y 20) (n 6))
  (declare (optimize (debug 3)))
  (declare (ignore a b y n))
  (let ((counter (make-instance 'counter))
	(s (make-instance 'vertex :label 'S))
	(*idx* 0))
    (with-productions ((p1 (p1 s counter)))
      (release counter)
      (with-productions ((p2a (p2 (left (vertex p1)) counter))
			 (p2b (p2 (right (vertex p1)) counter)))
	(release counter)
	(draw s)
	(with-productions ((p2c (p2 (left (vertex p2a)) counter))
			   (p2d (p2 (right (vertex p2a)) counter)))
	  (release counter)
	  (make-nodes (list p2c p2d p2b));no threads inside
	  ;(release counter)
	  (format t "~&Matrixes - Done~%")
	  (format t "~&Scalanie~%")
	  (loop
	     for group in `((,p2c ,p2d ,p2b)
			    (,p2a ,p2b)
			    (,p1))
	     do (map nil (lambda (node)
			   (print (label (vertex node)))
			   (print node)
			   (concat-nodes node)
			   ;(unless (eq (label (vertex node))
			   ; 'root)
			   (gaussian-elimination (vertex node)));)
			 group))
	    (format t "~&Scalanie - DONE~%")
	    (format t "~&Backward substitution~%")
	    (draw s))))
    s))

