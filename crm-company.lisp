(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)
(clsql:def-view-class crm-company ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
   (name
    :type (string 255)
    :initarg :name)
   (address
    :type (string 512)
    :initarg :address)

 (created-by
    :TYPE INTEGER
    :INITARG :created-by)
   (user-created-by
    :ACCESSOR company-created-by
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-users
                          :HOME-KEY created-by
                          :FOREIGN-KEY row-id
                          :SET NIL))
   (updated-by
    :TYPE INTEGER
    :INITARG :updated-by)
   (user-updated-by
    :ACCESSOR company-updated-by
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-users
                          :HOME-KEY updated-by
                          :FOREIGN-KEY row-id
                          :SET NIL))
   (deleted-state
    :type (string 1)
    :void-value "N"
    :initarg :deleted-state)
  
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class crm-users
                          :home-key row-id
                          :foreign-key tenant-id
                          :set t)))
  (:base-table crm_company))


(defun new-crm-company(cname caddress)
  (let  ((company-name cname)(company-address caddress))
	(clsql:update-records-from-instance (make-instance 'crm-company
							   :name company-name
							   :address company-address
							   :deleted-state "N"
							   :created-by (get-login-tenant-id)
							   :updated-by (get-login-tenant-id)))))


(defun get-login-tenant-id ()
  (slot-value  (get-login-user-object (get-current-login-username)) 'tenant-id))


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


(defun crm-controller-delete-company ()
(if (is-crm-session-valid?)
    (let ((id (hunchentoot:parameter "id")) )
      (delete-crm-company id)
      (hunchentoot:redirect "/list-companies"))
     (hunchentoot:redirect "/login")))


(defun crm-controller-list-companies ()
(if (is-crm-session-valid?)
   (let (( companies (list-crm-companies)))
    (standard-page (:title "List companies")
      (:table :cellpadding "0" :cellspacing "0" :border "1"
     (loop for company in companies
       do (htm (:tr (:td :colspan "3" :height "12px" (str (slot-value company 'name)))
		    (:td :colspan "12px" (:a :href  (format nil  "/delcomp?id=~A" (slot-value company 'row-id)) "Delete"))
		    
		    ))))))
 (hunchentoot:redirect "/login")))


