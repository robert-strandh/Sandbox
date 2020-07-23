(cl:in-package #:sandbox)

(defun register-lexical (lexical table create-p)
  (unless (nth-value 1 (gethash lexical table))
    (setf (gethash lexical table) create-p)))

(defgeneric gather-lexicals (ast table))

(defmethod gather-lexicals (ast table)
  (loop for child in (cleavir-ast:children ast)
        do (gather-lexicals child table)))

(defmethod gather-lexicals ((ast cleavir-ast:function-ast) table)
  (loop for item in (cleavir-ast:lambda-list ast)
        do (cond ((member item lambda-list-keywords)
                  nil)
                 ((atom item)
                  (register-lexical item table nil))
                 (t (loop for lexical in item
                          do (register-lexical lexical table nil)))))
  (call-next-method))

(defmethod gather-lexicals ((ast cleavir-ast:setq-ast) table)
  (register-lexical (cleavir-ast:lhs-ast ast) table t))
