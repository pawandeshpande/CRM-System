(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)


(defun new-crm-journal-entry(name description  tenant-id user-id )
    (clsql:update-records-from-instance (make-instance 'crm-journal-entry
				    :name name
				    :description description
				    :deleted-state "N"
				    :tenant-id tenant-id
				    :created-by user-id
				    :updated-by user-id)))


(defun new-crm-drcr-entry (journal-entry-id account-id debit credit)
(clsql:update-records-from-instance (make-instance 'crm-journal-debit-credit-entry
						    :acct-id account-id
						   :opty-id journal-entry-id
						   :tenant-id (get-login-tenant-id)
				   :amount (resolve-drcr debit credit) 
				    :created-by (get-login-tenant-id)   
				    :updated-by (get-login-tenant-id))))



(defun resolve-drcr (dr cr)
	      (cond ((not (or (= 0 dr) (= 0 cr))) (error "one number must be zero"))
		    ((and (= 0 dr) (= 0 cr)) (error "Both numbers cannot be zero"))
		    ((= dr 0 ) cr)
		    ((= cr 0 ) dr)))

(defun crm-journal-entry-with-drcr (name description  SrcAct DstAct SrcDr SrcCr DstDr DstCr tenant-id user-id)
  (progn (new-crm-journal-entry name description tenant-id user-id)
		 (new-crm-drcr-entry (get-journal-entry-id name tenant-id)  srcAct SrcDr SrcCr)
		 (new-crm-drcr-entry (get-journal-entry-id name tenant-id)  DstAct DstDr DstCr)))	

(defun get-journal-entry (name tenant-id)
  (car (clsql:select 'crm-journal-entry :where [and [=[:name] name]
		[=[:tenant-id] tenant-id]] :caching nil :flatp t)))

(defun get-journal-entry-id (name tenant-id)
  (car (clsql:select [row-id] :from [crm_opportunity] :where [and [=[:name] name]
		[=[:tenant-id] tenant-id]] :caching nil :flatp t)))


(defun list-crm-journal-entries (tenant-id)
  (clsql:select 'crm-journal-entry  :where [and [= [:deleted-state] "N"] [= [:tenant-id] tenant-id]]   :caching nil :flatp t ))

(defun list-crm-journal-entry-details (opty-id tenant-id)
  (clsql:select 'crm-journal-debit-credit-entry :where [and [= [:tenant-id] tenant-id]
		[= [:opty-id] opty-id]] :caching nil :flatp t))

(defun delete-crm-journal-debit-credit-entry (opty-id)
  (let ((journal-entry-detail (car (clsql:select 'crm-journal-debit-credit-entry :where [= [:opty-id] opty-id] :flatp t :caching nil))))
    (setf (slot-value journal-entry-detail 'deleted-state) "Y")
    (clsql:update-record-from-slot journal-entry-detail 'deleted-state)))
  

(defun delete-crm-journal-entry ( id )
  (let ((journal-entry (car (clsql:select 'crm-journal-entry :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value journal-entry 'deleted-state) "Y")
    (clsql:update-record-from-slot journal-entry 'deleted-state)))
    

(defun delete-crm-journal-entries ( list )
  (mapcar (lambda (id)  (let ((journal-entry (car (clsql:select 'crm-journal-entry :where [= [:row-id] id] :flatp t :caching nil))))
			  (setf (slot-value journal-entry 'deleted-state) "Y")
			  (clsql:update-record-from-slot journal-entry  'deleted-state))) list ))


(defun restore-deleted-crm-journal-entries ( list )
(mapcar (lambda (id)  (let ((journal-entry (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value journal-entry 'deleted-state) "N")
    (clsql:update-record-from-slot journal-entry 'deleted-state))) list ))

