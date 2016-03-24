(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)


(defun list-crm-users ()
  (clsql:select 'crm-users  :where [and [= [:deleted-state] "N"] [= [:tenant-id] (get-login-tenant-id)]]    :caching nil :flatp t ))

(defun delete-crm-user ( id )
  (let ((crmuser (car (clsql:select 'crm-users :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value crmuser 'deleted-state) "Y")
    (clsql:update-record-from-slot crmuser 'deleted-state)))
    

(defun delete-crm-users ( list )
  (mapcar (lambda (id)  (let ((crmuser (car (clsql:select 'crm-users :where [= [:row-id] id] :flatp t :caching nil))))
			  (setf (slot-value crmuser 'deleted-state) "Y")
			  (clsql:update-record-from-slot crmuser  'deleted-state))) list ))


(defun restore-deleted-crm-users ( list )
(mapcar (lambda (id)  (let ((crmuser (car (clsql:select 'crm-users :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value crmuser 'deleted-state) "N")
    (clsql:update-record-from-slot crmuser 'deleted-state))) list ))


(defun verify-superadmin ();;"Verifies whether username is superadmin" 
  (if (equal (get-current-login-username) "superadmin") T NIL ))

(defun superadmin-login (company-id)
(if (verify-superadmin )
  (setf ( hunchentoot:session-value :login-company)   (get-tenant-name company-id))))

	    

  
(defun create-crm-user(name uname passwd email-address tenant-id )
 (clsql:update-records-from-instance (make-instance 'crm-users
				    :name name
				    :username uname
				    :password passwd
				    :email email-address
				    :tenant-id tenant-id
				    :deleted-state "N"
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id))))
 
