(in-package :crm-system)
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
    (if ( is-crm-session-valid?)
	;; if session is valid then go ahead and create the company
    (clsql:update-records-from-instance (make-instance 'crm-company
				    :name company-name
				    :address company-address
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id)))
     ;; else redirect to the login page
    (hunchentoot:redirect "/login"))))


(defun get-login-tenant-id ()
  (slot-value  (get-login-user-object (get-current-login-user)) 'tenant-id))
