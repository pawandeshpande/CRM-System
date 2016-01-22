(in-package :crm-system)
(clsql:def-view-class crm-users ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg row-id)
   (NAME
    :accessor name
    :DB-CONSTRAINTS :NOT-NULL
    :TYPE (string 30)
    :INITARG :name)
   (username
    :ACCESSOR username 
    :type (string 30)
    :initarg :username)
   (password
    :accessor password
    :type (string 30)
    :initarg :password)
   (email
    :accessor email
    :type (string 255)
    :initarg :email)
   (created-by
    :TYPE INTEGER
    :INITARG :created-by)
   (user-created-by
    :ACCESSOR user-created-by
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-users
                          :HOME-KEY created-by
                          :FOREIGN-KEY row-id
                          :SET NIL))
   (updated-by
    :TYPE INTEGER
    :INITARG :updated-by)
   (user-updated-by
    :ACCESSOR user-updated-by
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-users
                          :HOME-KEY updated-by
                          :FOREIGN-KEY row-id
                          :SET NIL))
   (tenant-id
    :type integer
    :initarg :tenant-id)
   (COMPANY
    :ACCESSOR users-company
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-company
	                  :HOME-KEY tenant-id
                          :FOREIGN-KEY row-id
                          :SET NIL))

   
   (parent-id
    :type integer
    :initarg :parent-id)
   (manager
    :accessor users-manager
    :db-kind :join
    :db-info (:join-class crm_users
                          :home-key parent-id
                          :foreign-key row-id
                          :set nil)))

   
  (:BASE-TABLE crm_users))




(defun new-crm-user(name uname passwd email-address )
 (if ( is-crm-session-valid?)
	;; if session is valid then go ahead and create the company
    (clsql:update-records-from-instance (make-instance 'crm-users
				    :name name
				    :username uname
				    :password passwd
				    :email email-address
				    :created-by (slot-value  (get-login-user-object (get-current-login-user)) 'tenant-id)
				    :updated-by (slot-value  (get-login-user-object (get-current-login-user)) 'tenant-id)))
     ;; else redirect to the login page
    (hunchentoot:redirect "/login")))

(defun get-crm-roles ()
 ())

