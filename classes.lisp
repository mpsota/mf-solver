(in-package #:mf-solver)

(defclass matrix()
  ())

(defclass vertex ()
  ((left :accessor left :initarg :left  :initform nil)
   (right :accessor right :initarg :right  :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (label :accessor label :initarg :label )
   ;(leaf? :accessor leaf? :initarg :leaf? )
   ;(root? :accessor root? :initarg :root? )
   ;(name :accessor name :initarg :name )
   ;(level :initarg :level :initform 0)
   (m :accessor m :initarg :m :initform (make-array '(3 3) :initial-element 0))
   (v :accessor v :initarg :v :initform (make-array '(3) :initial-element 0))))

(defclass executor ()
  ; parametry: np.: u(a) = 0, u(b) = 20 (y)
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b)
   (y :initarg :y :accessor y)
   (n :initarg :n :accessor n)
   (h :accessor h)
   (leafs :accessor leafs)))

(defclass counter()
  ((counter :accessor counter :initarg :counter :initform 0)
   (notify :accessor notify :initarg :notify :initform 'notify)))

(defclass production()
  ((vertex :accessor vertex :initarg :vertex)
   (counter :accessor counter :initarg :counter)))

(defclass p1(production)
  ())

(defclass p2(production)
  ())

(defclass p3(production)
  ())

(defclass a1(production)
  ())

(defclass a2(production)
  ())
