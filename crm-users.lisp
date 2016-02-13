(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)
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

   (deleted-state
    :type (string 1)
    :void-value "N"
    :initarg :deleted-state)

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




(defun new-crm-user(name uname passwd email-address tenant-id )
 (if ( is-crm-session-valid?)
	;; if session is valid then go ahead and create the company
    (clsql:update-records-from-instance (make-instance 'crm-users
				    :name name
				    :username uname
				    :password passwd
				    :email email-address
				    :tenant-id tenant-id
				    :deleted-state "N"
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id)))
     ;; else redirect to the login page
    (hunchentoot:redirect "/login")))


  
(defun crm-controller-list-users ()
(if (is-crm-session-valid?)
   (let (( crmusers (list-crm-users)))
     (standard-page (:title "List CRM Users")
       (:h3 "Users")

      (:table :class "table table-striped"  
	      (:thead (:tr (:th "User Name") (:th "Action")))(:tbody
     (loop for crmuser in crmusers
       do (htm (:tr (:td  :height "12px" (str (slot-value crmuser 'name)))
		    (:td :height "12px" (:a :href  (format nil  "/deluser?id=~A" (slot-value crmuser 'row-id)) "Delete")))))))))
   (hunchentoot:redirect "/login")))

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

