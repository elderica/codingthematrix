(defsystem :codingthematrix
  :author "elderica"
  :description "Coding the Matrix with Common Lisp"
  :license "The Unlicense"
  :depends-on (:uiop :cl-utilities)
  :pathname "src/"
  :components ((:file "inverse-index")
               (:file "plotting")))
