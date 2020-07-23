(cl:in-package #:sandbox)

(defclass client (trucler-reference:client)
  ())

(defun translate-form (form env)
  (let* ((client (make-instance 'client))
         (cst (cst:cst-from-expression form))
         (ast (cleavir-cst-to-ast:cst-to-ast client cst env))
         (table (make-hash-table :test #'eq))
         (var (gensym)))
    (gather-lexicals ast table)
    `(lambda (,var)
       (declare (ignorable ,var))
       (let ,(loop for ast being each hash-key of table using (hash-value create-p)
                   when create-p collect (cleavir-ast:name ast))
         ,(translate-ast ast var '())))))

(defun evaluate-form (form env)
  (funcall (compile nil (translate-form form env)) env))

(defun repl ()
  (let ((env (make-instance 'sicl-simple-environment:simple-environment)))
    (fill-environment env)
    (sicl-genv:fmakunbound 'cadr env)
    (loop do (format t "SANDBOX> ")
             (finish-output)
             (print (evaluate-form (read) env))
             (terpri))))


  
