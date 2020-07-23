(cl:in-package #:sandbox)

(defgeneric translate-ast (ast env dico))

(defmethod translate-ast
    ((ast cleavir-ast:fdefinition-ast) env dico)
  `(sicl-genv:fdefinition
    ,(translate-ast (cleavir-ast:name-ast ast) env dico)
    ,env))

(defmethod translate-ast
    ((ast cleavir-ast:constant-ast) env dico)
  `',(cleavir-ast:value ast))

(defmethod translate-ast
    ((ast cleavir-ast:lexical-ast) env dico)
  (cleavir-ast:name ast))

(defmethod translate-ast
    ((ast cleavir-ast:symbol-value-ast) env dico)
  `(symbol-value
    ,(translate-ast (cleavir-ast:name-ast ast) env dico)))

(defmethod translate-ast
    ((ast cleavir-ast:set-symbol-value-ast) env dico)
  `(setf (symbol-value
          ,(translate-ast (cleavir-ast:name-ast ast) env dico))
         ,(translate-ast (cleavir-ast:value-ast ast) env dico)))

(defmethod translate-ast
    ((ast cleavir-ast:call-ast) env dico)
  `(funcall
    ,(translate-ast (cleavir-ast:callee-ast ast) env dico)
    ,@(loop for argument-ast in (cleavir-ast:argument-asts ast)
            collect (translate-ast argument-ast env dico))))

(defmethod translate-ast
    ((ast cleavir-ast:function-ast) env dico)
  `(lambda ,(loop for item in (cleavir-ast:lambda-list ast)
                  collect (cond ((member item lambda-list-keywords)
                                 item)
                                ((atom item)
                                 (cleavir-ast:name item))
                                (t
                                 (mapcar #'cleavir-ast:name item))))
     ,(translate-ast (cleavir-ast:body-ast ast) env dico)))

(defmethod translate-ast
    ((ast cleavir-ast:progn-ast) env dico)
  `(progn ,@(loop for form-ast in (cleavir-ast:form-asts ast)
                  collect (translate-ast form-ast env dico))))

(defmethod translate-ast
    ((ast cleavir-ast:block-ast) env dico)
  (let ((name (gensym)))
    `(block ,name
       ,(translate-ast
         (cleavir-ast:body-ast ast)
         env
         (push (cons ast name) dico)))))

(defmethod translate-ast
    ((ast cleavir-ast:return-from-ast) env dico)
  `(return-from ,(cdr (assoc (cleavir-ast:block-ast ast) dico))
     ,(translate-ast (cleavir-ast:form-ast ast) env dico)))

(defmethod translate-ast
    ((ast cleavir-ast:setq-ast) env dico)
  `(setq ,(translate-ast (cleavir-ast:lhs-ast ast) env dico)
         ,(translate-ast (cleavir-ast:value-ast ast) env dico)))

(defmethod translate-ast
    ((ast cleavir-ast:if-ast) env dico)
  `(if ,(translate-ast (cleavir-ast:test-ast ast) env dico)
       ,(translate-ast (cleavir-ast:then-ast ast) env dico)
       ,(translate-ast (cleavir-ast:else-ast ast) env dico)))
