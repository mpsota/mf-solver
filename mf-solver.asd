;;;; mf-solver.asd

(asdf:defsystem #:mf-solver
  :name "Multifrontal solver"
  :author "Michal Psota"
  :version "0.0"
  :depends-on (:trees :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "classes")
	       (:file "mf-solver")))






