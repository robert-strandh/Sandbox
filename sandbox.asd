(cl:in-package #:asdf-user)

(defsystem #:sandbox
  :depends-on (#:sicl-simple-environment
               #:cleavir2-ast
               #:cleavir2-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "translate-ast")
   (:file "gather-lexicals")
   (:file "fill-environment")
   (:file "evaluate-form")))
