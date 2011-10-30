;;;; mf-solver.lisp

(in-package #:mf-solver)

;;; "mf-solver" goes here. Hacks and glory await!

(defvar *mutex* (sb-thread:make-mutex :name "my lock"))
(defvar *wait-queue* (sb-thread:make-waitqueue ))

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
      (sb-thread:condition-notify *wait-queue*))))

(defmethod release ((c counter))
  (sb-thread:with-mutex (*mutex*)
    (format t "~&Wait~%")
    (sb-thread:condition-wait *wait-queue* *mutex*)
    (format t "~&Release~%")))


;; vertex

(defmethod app((p p1) (s Vertex))
  (format t "~A~%" 'p1)
  (let ((t1 (make-instance 'vertex :left nil :right nil :parent s :label 't))
	(t2 (make-instance 'vertex :left nil :right nil :parent s :label 't)))
    (setf (left S) t1)
    (setf (right S) t2)
    (setf (label s) 'root))
  s)

(defmethod app((p p2) (tt Vertex))
  (format t "~A~%" 'p2)
  (let ((t1 (make-instance 'vertex :left nil :right nil :parent tt :label 't))
	(t2 (make-instance 'vertex :left nil :right nil :parent tt :label 't)))
    (setf (left tt) t1)
    (setf (right tt) t2)
    (setf (label tt) 'int))
  tt)

(defmethod app((p p3) (tt Vertex))
  (format t "~A~%" 'p3)
  (let ((t1 (make-instance 'vertex :left nil :right nil :parent tt :label 'node))
	(t2 (make-instance 'vertex :left nil :right nil :parent tt :label 'node)))
    (setf (left tt) t1)
    (setf (right tt) t2)
    (setf (label tt) 'int))
  tt)

(defmethod app((p a1) (tt Vertex))
  (format t "~A~%" 'a1)
  (setf (aref (m tt) 1 1)  1
	(aref (m tt) 2 1)  1
	(aref (m tt) 1 2)  0
	(aref (m tt) 2 2) -1)
  (setf (aref (v tt) 1) 0
	(aref (v tt) 2) 0)  
  tt)

(defmethod app((p a2) (tt Vertex))
  (format t "~A~%" 'a2)
  (setf (aref (m tt) 0 0) (+ (aref (m (left tt)) 2 2)
			     (aref (m (right tt)) 1 1))
	(aref (m tt) 1 0) (aref (m (left tt)) 1 2)
	(aref (m tt) 2 0) (aref (m (right tt)) 2 1)
	(aref (m tt) 0 1) (aref (m (left tt)) 2 1)
	(aref (m tt) 1 1) (aref (m (left tt)) 1 1)
	(aref (m tt) 2 1) 0
	(aref (m tt) 0 2) (aref (m (right tt)) 1 2)
	(aref (m tt) 1 2) 0
	(aref (m tt) 2 2) (aref (m (left tt)) 2 2))
  (setf (aref (v tt) 0) 0
	(aref (v tt) 1) 0
	(aref (v tt) 2) 0)  
  tt)

;; production

(defmethod start((p production))
  (inc (counter p))
  (sleep (random 7))
  (app p (vertex p))
  (draw  (vertex p))
  (dec (counter p)))

(defmethod initialize-instance :after ((p production) &rest initargs) 
  (declare (ignore initargs))
  #+nil(sb-thread:make-thread  #'start :arguments p))
  
;;;;
;;;; Print tree
;;;;

(defun pprint-tree (V &optional (stream *standard-output*))
  (labels ((recursive-print (node level char)
             (format t "~Vt~A ~A~%" level char (label node))
             (when (left node)
               (recursive-print (left node) (1+ level) #\l))
             (when (right node)
               (recursive-print (right node) (1+ level) #\r))))
    (if t;(eq (label v) 'root)
	(recursive-print v 0 #\R)
	(format t "~&not root!~%"))))

(defmethod draw ((v vertex))
  (pprint-tree v)
  #+nil(labels ((plot (v)
	     (format t "~A " (label v))
	     (when (left v)
	       (format t "Left son:")
	       (plot (left v)))
	     (when (right v)
	       (format t "Right son:")
	       (plot (right v)))
	     (format t ".~%")))
    (loop
       while (parent v)
       do
	 (setf v (parent v))
       finally (plot v))))

;;;;
;;;; entry point
;;;;

(defmacro with-productions (list &body body)
  (let ((old-io (gensym "old-io")))
    `(let 
       ,(loop
	   for i from 0 
	   for production in list collect
	     (destructuring-bind (name (production vertex counter)) production
	       `(,name (make-instance ',production :vertex ,vertex :counter ,counter))))
       (let ((,old-io *standard-output*))
	 ,@(loop
	      for production in list
	      for var = (car production) collect
		`(sb-thread:make-thread  (lambda ()
					   (let ((*standard-output* ,old-io))
					     (start ,var))))))
       ,@body)))

#+nil(with-productions ((p2a (p2 (left (vertex p1)) counter))
			(p2b (p2 (right (vertex p1)) counter))))
  
  

(defun main(&key (a 0) (b 1) (y 20) (n 6))
  (declare (ignore a b y n))
  (let ((counter (make-instance 'counter ))
	(s (make-instance 'vertex :label 'S)))
    (with-productions ((p1 (p1 s counter)))
      (release counter)
      (with-productions ((p2a (p2 (left (vertex p1)) counter))
			 (p2b (p2 (right (vertex p1)) counter)))
	(release counter)
	(draw s)
	(with-productions ((p2c (p2 (left (vertex p2a)) counter))
			   (p2d (p2 (right (vertex p2a)) counter))
			   (p3a (p3 (left (vertex p2b)) counter))
			   (p3b (p3 (right (vertex p2b)) counter)))
	  (release counter)
	  (with-productions ((p3c (p3 (left (vertex p2c)) counter))
			     (p3d (p3 (right (vertex p2c)) counter))
			     (p3e (p3 (left (vertex p2d)) counter))
			     (p3f (p3 (right (vertex p2d)) counter)))
	    (release counter)
	    (format t "~&Done~%")
	    (draw s)))))
    s))


