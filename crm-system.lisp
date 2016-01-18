
(defpackage :crm-system
  (:use :cl :cl-who :hunchentoot :clsql))



;; You must set these variables to appropriate values.
(defvar *crm-database-type* :odbc
  "Possible values are :postgresql :postgresql-socket, :mysql,
:oracle, :odbc, :aodbc or :sqlite")
(defvar *crm-database-name* "TestCRMCore"
  "The name of the database we will work in.")
(defvar *crm-database-user* "TestCRMCore"
  "The name of the database user we will work as.")
(defvar *crm-database-server* "localhost"
  "The name of the database server if required")
(defvar *crm-database-password* "TestCRMCore"
  "The password if required")




;; Connect to the database (see the CLSQL documentation for vendor
;; specific connection specs).

(defun crm-db-connect(&key strdb strusr strpwd servername strdbtype)

(progn 
  (case strdbtype
  ((:mysql :postgresql :postgresql-socket)
   (clsql:connect `(,servername
                    ,strdb
                    ,strusr
                    ,strpwd)
                  :database-type strdbtype))
  ((:odbc :aodbc :oracle)
   (clsql:connect `(,strdb
                    ,strusr
                    ,strpwd)
                  :database-type strdbtype))
  (:sqlite
   (clsql:connect `(,strdb)
                  :database-type strdbtype)))

  (clsql:start-sql-recording)
;; lets use the functional sql interface
(clsql:locally-enable-sql-reader-syntax)))

;; Connect to the database.
(crm-db-connect :strdb *crm-database-name* :strusr *crm-database-user* :strpwd *crm-database-password* :strdbtype *crm-database-type*)


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
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class crm-users
                          :home-key row-id
                          :foreign-key tenant-id
                          :set t)))
  (:base-table crm_company))





(defun create-company (company-instance)
  (clsql:create-view-from-class company-instance))

(defvar *companies* '())

(defun companies ()
  (sort (copy-list  *companies*) #'> :key #'name)) 


(defmacro standard-page ((&key title) &body body)
	 `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	   (:html :xmlns "http://www.w3.org/1999/xhtml"
		  :xml\:lang "en" 
		  :lang "en"
		  (:head 
		   (:meta :http-equiv "Content-Type" 
			  :content    "text/html;charset=utf-8")
		   (:title ,title)
		   (:link :type "text/css" 
			  :rel "stylesheet"
			  :href "./crm-system.css"))
		  (:body 
		   (:div :id "header" ; CRM System header
			 (:img :src "crm.jpg" 
			       :alt "CRM" 
			       :class "logo")
			 (:span :class "strapline" 
				"Welcome to CRM System"))
		   ,@body))))



(defun crm-controller-index () 
	 (standard-page (:title "Welcome to CRM World")
			(:h1 "CRM World")
			(:p "Want to create a new company?" (:a :href "/new-company" "here"))
			(:h2 "Current stand")
			(:div :id "chart" ; For CSS styling of links
			      (:ol
			       (dolist (company (companies))
				 (htm  
				  (:li 
				   (:a :href (format nil "vote?name=~a" (name company)) "Vote!")
				   (fmt "~A with ~d votes" (name company) (address company game)))))))))




(defun crm-controller-new-company ()
 	 (standard-page (:title "Add a new company")
			(:h1 "Add a new company")
			(:form :action "/company-added" :method "post" 
			       (:p "What is the name of the company?" (:br)
				   (:input :type "text"  
					   :name "name" 
					   :class "txt"))
			       (:p (:input :type "submit" 
					   :value "Add" 
					   :class "btn")))))





(defun controller-company-added ()
  (let  ((name (parameter "name")))
  (unless ( or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/index")))


(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-regex-dispatcher "^/crmindex" 'crm-controller-index)
       (hunchentoot:create-regex-dispatcher "^/company-added" 'crm-controller-company-added)
       (hunchentoot:create-regex-dispatcher "^/new-company" 'crm-controller-new-company)))

(clsql:def-view-class crm-account ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
   (account-no
    :type (string 10)
    :db-constraints :not-null
    :initarg :account-no)
   (account-name
    :type (string 30)
    :db-constraints :not-null
    :initarg :name)
   (account-description
    :type (string 100)
    :initarg :description)
   
   )
(:base-table crm_account))




(defvar TestAdmin1 (car (clsql:select 'crm-users :where [= [slot-value 'crm-users 'username] "TestAdmin1"]
				:flatp t)))


(defvar TestCompany1 (car (clsql:select 'crm-company :where [= [slot-value 'crm-company 'name] "TestCompany"]
				:flatp t)))



;;(defvar TestUser1 (make-instance 'crm-users
;;				 :name "Test User1"
;;				 :username "testuser1"
;;				 :password "P@ssword1"
;;				 :created-by 1
;;				 :updated-by 1
;;				 :tenant-id 1
;;				 :parent-id 1
;;				 ))
;;(clsql:update-records-from-instance TestUser1)




