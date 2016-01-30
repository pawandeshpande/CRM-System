(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class crm-account ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
  ; not using currently
  ; (account-no
  ;  :type (string 10)
  ;  :db-constraints :not-null
  ;  :initarg :account-no)
   (name
    :type (string 30)
    :db-constraints :not-null
    :initarg :name)
   (description
    :type (string 100)
    :initarg :description)

   (account-type
    :type integer
    :db-constraints :not-null
    :initarg :account-type)
   (type-of-account
    :accessor type-of-account
    :db-kind :join
    :db-info (:join-class crm-account-type
			  :home-key account-type
			  :foreign-key row-id
			  :set nil))
   
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

   (tenant-id
    :type integer
    :initarg :tenant-id)
   (COMPANY
    :ACCESSOR account-company
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-company
	                  :HOME-KEY tenant-id
                          :FOREIGN-KEY row-id
                          :SET NIL))

   
   (deleted-state
    :type (string 1)
    :void-value "N"
    :initarg :deleted-state))
  (:base-table crm_account))


(clsql:def-view-class crm-account-type ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
   (name
    :type (string 50)
    :db-constraints :not-null
    :initarg :name)
    (description
    :type (string 255)
    :initarg :description))
(:base-table crm_account_type))




(defun new-crm-account(name description acct-type tenant-id )
    (clsql:update-records-from-instance (make-instance 'crm-account
				    :name name
				    :description description
				    :account-type acct-type
				    :tenant-id tenant-id
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id))))

(defun list-crm-accounts ()
  (clsql:select 'crm-account  :where [= [:deleted-state] "N"]   :caching nil :flatp t ))

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


(defun crm-controller-delete-account ()
(if (is-crm-session-valid?)
    (let ((id (hunchentoot:parameter "id")) )
      (delete-crm-account id)
      (hunchentoot:redirect "/list-accounts"))
     (hunchentoot:redirect "/login")))


(defun crm-controller-list-accounts ()
(if (is-crm-session-valid?)
   (let (( accounts (list-crm-accounts)))
    (standard-page (:title "List Accounts")
      (:table :cellpadding "0" :cellspacing "0" :border "1"
     (loop for account in accounts
       do (htm (:tr (:td :colspan "3" :height "12px" (str (slot-value account 'name)))
		    (:td :colspan "12px" (:a :href  (format nil  "/delaccount?id=~A" (slot-value account 'row-id)) "Delete"))
		    
		    ))))))
   (hunchentoot:redirect "/login")))

(defparameter *crm-account-types* (clsql:select [:name] :from 'crm-account-type :caching nil :flatp t))

(defmacro account-type-dropdown ()
  `(cl-who:with-html-output (*standard-output* nil)
     (let ((count 0))
     (htm (:select :name "accounttype"  
      (loop for acct-type in *crm-account-types*
	 do (htm  (:option :value (incf count) (str acct-type)))))))))


(defun crm-controller-new-account ()
  (if (is-crm-session-valid?)
      (standard-page (:title "Add a new Account")
	(:h1 "Add a new Account")
	(:form :action "/account-added" :method "post" 
	       (:p "Name: "
		   (:input :type "text"  
			   :name "name" 
			   :class "txt")
		   (:p "Description: " (:input :type "text"  
					    :name "username" 
					    :class "txt"))
		   (:p "Account Type: " (account-type-dropdown))
		   
		   ;; Add a drop down list of available roles for the user.
		   (:p (:input :type "submit" 
			       :value "Add" 
			       :class "btn")))))
      (hunchentoot:redirect "/login")))



	   
(defun crm-controller-account-added ()
  (if (is-crm-session-valid?)
      (let  ((name (hunchentoot:parameter "name"))
	     (description (hunchentoot:parameter "description"))
	     (accounttype (hunchentoot:parameter "accounttype")))
	     
	(unless(and  ( or (null name) (zerop (length name)))
		     ( or (null description) (zerop (length description)))
		     ( or (null accounttype) (zerop (length accounttype))))
	  
	  (new-crm-account name description (parse-integer accounttype) (get-login-tenant-id) ))

	(hunchentoot:redirect  "/crmindex"))
      (hunchentoot:redirect "/login")))


