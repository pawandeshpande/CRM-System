(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)



(defun new-crm-account(name description acct-type tenant-id )
(let ((account-instance (make-instance 'crm-account
				    :name name
				    :description description
				    :account-type acct-type
				    :deleted-state "N"
				    
				    :tenant-id tenant-id
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id))))
  
  (clsql:update-records-from-instance account-instance)))

(defun list-current-login-crm-accounts ()
  (clsql:select 'crm-account  :where [and [= [:deleted-state] "N"] [= [:tenant-id] (get-login-tenant-id)]]   :caching nil :flatp t ))

(defun list-crm-accounts ( company-id)
  (clsql:select 'crm-account  :where [and [= [:deleted-state] "N"] [= [:tenant-id] company-id]]   :caching nil :flatp t ))


(defun delete-crm-account ( id )
  (let ((account (car (clsql:select 'crm-account :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value account 'deleted-state) "Y")
    (clsql:update-record-from-slot account 'deleted-state)))
    

(defun delete-crm-accounts ( list )
  (mapcar (lambda (id)  (let ((account (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
			  (setf (slot-value account 'deleted-state) "Y")
			  (clsql:update-record-from-slot account  'deleted-state))) list ))


(defun restore-deleted-crm-accounts ( list )
(mapcar (lambda (id)  (let ((account (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value account 'deleted-state) "N")
    (clsql:update-record-from-slot account 'deleted-state))) list ))

