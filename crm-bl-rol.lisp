(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)


(defun clone-crm-roles ()
  (let (( *company-roles*  (clsql:select 'crm-roles :where [= [slot-value 'crm-roles 'tenant-id] 1]
					     :flatp t)))
    (loop for role  in *company-roles*
       do ( setf (slot-value role 'tenant-id) (get-login-tenant-id))
	 (clsql:update-records-from-instance role))))




(defun html-select-roles (stream)
    (cl-who:with-html-output (stream)
      (:select (:option :value "pawan")
	       (:option :value "pawan2" :selected "selected" ))))
