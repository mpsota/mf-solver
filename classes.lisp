(in-package #:mf-solver)

(defclass vertex ()
  ((left :accessor left :initarg :left  :initform nil)
   (right :accessor right :initarg :right  :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (label :accessor label :initarg :label)
   (m :accessor m :initarg :m :initform (make-array '(3 3) :initial-element 0))
   (v :accessor v :initarg :v :initform (make-array '(3) :initial-element 0))
   (idx :accessor idx :initarg :idx :initform (make-array '(3) :initial-element nil))))

(defclass counter()
  ((counter :accessor counter :initarg :counter :initform 0)
   (notify :accessor notify :initarg :notify :initform 'notify)))

(defclass production()
  ((p-type :accessor p-type :initarg :p-type)
   (vertex :accessor vertex :initarg :vertex)
   (counter :accessor counter :initarg :counter)))

