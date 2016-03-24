(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

(defun new-crm-company(cname caddress createdby updatedby)
  (let  ((company-name cname)(company-address caddress))
	(clsql:update-records-from-instance (make-instance 'crm-company
							   :name company-name
							   :address company-address
							   :deleted-state "N"
							   :created-by createdby
							   :updated-by updatedby))))



(defun list-crm-companies ()
  (clsql:select 'crm-company  :where [= [:deleted-state] "N"]   :caching nil :flatp t ))

(defun delete-crm-company ( id )
  (let ((company (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value company 'deleted-state) "Y")
    (clsql:update-record-from-slot company 'deleted-state)))
    

(defun delete-crm-companies ( list )
  (mapcar (lambda (id)  (let ((company (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
			  (setf (slot-value company 'deleted-state) "Y")
			  (clsql:update-record-from-slot company 'deleted-state))) list ))


(defun restore-deleted-crm-companies ( list )
(mapcar (lambda (id)  (let ((company (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value company 'deleted-state) "N")
    (clsql:update-record-from-slot company 'deleted-state))) list ))



