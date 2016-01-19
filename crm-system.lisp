(defpackage :crm-system
  (:use :cl :cl-who :hunchentoot :clsql))

(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

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

(defvar *logged-in-users* nil)
(defvar *current-user-session* nil)


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
  (clsql:enable-sql-reader-syntax)))

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
			 (:img :src "crm-logo.jpg" 
			       :alt "CRM" 
			       :class "logo")
			 (:span :class "strapline" 
				"Welcome to CRM System"))
		   ,@body))))



(defun crm-controller-index () 
	 (standard-page (:title "Welcome to CRM World")
			(:h1 "CRM World") 
			(:p "Want to create a new company?" (:a :href "/new-company" "here"))
			
	 (:a :href "/crmlogout" "Logout")))

(setq *logged-in-users* (make-hash-table :test 'equal))


(defun crm-controller-loginpage ()
   (standard-page (:title "Welcome to CRM System")
		 (:h1 "Login to CRM System")
			(:form :action "/crmlogin" :method "post" 
			       (:p "Company" (:br)
				   (:input :type "text"  
					   :name "company" 
					   :class "txt"))
			      
			       (:p "Username" (:br)
				   (:input :type "text"  
					   :name "username" 
					   :class "txt"))
			       (:p "Password" (:br)
				   (:input :type "password"  
					   :name "password" 
					   :class "password"))
			       (:p (:input :type "submit" 
					   :value "Add" 
					   :class "btn")))))


(defun crm-controller-login ()
   (let  ((uname (hunchentoot:parameter "username"))
	  (passwd (hunchentoot:parameter "password"))
	  (cname (hunchentoot:parameter "company")))
    
     (unless(and
	     ( or (null cname) (zerop (length cname)))
	     ( or (null uname) (zerop (length uname)))
		 ( or (null passwd) (zerop (length passwd))))
       (if (equal (crm-login :company-name cname :username uname :password passwd) NIL) (hunchentoot:redirect "/loginpage") (hunchentoot:redirect "/crmindex")))))

  
   (defun crm-controller-logout ()
     (progn (crm-logout (get-current-login-user))
			(hunchentoot:acceptor-remove-session 'hunchentoot:easy-acceptor 'hunchentoot:*session*)))


(defun get-current-login-user ()
  (hunchentoot:session-value :username))



(defun crm-login (&key company-name username password)
  (let ((login-user (car (clsql:select 'crm-users :where [and
				   [= [slot-value 'crm-users 'username] username]
				   [= [slot-value 'crm-users 'password] password]
				   [= [slot-value 'crm-users 'tenant-id] (get-tenant-id company-name )]]
				   :flatp t))))
    (if (equalp (slot-value login-user 'username) NIL) NIL (progn (add-login-user username  login-user)
    (setf *current-user-session* (hunchentoot:start-session))
    (setf (hunchentoot:session-value :login-username) username)))))


(defun get-tenant-id (company-name)
    ( car ( clsql:select [row-id] :from [crm-company] :where [= [slot-value 'crm-company 'name] company-name]
				    :flatp t)))

      
(defun get-login-user (username)
  (gethash username *logged-in-users*))


(defun is-user-already-login? (username)
 ( get-login-user username))


(defun add-login-user(username object)
  (unless (is-user-already-login? username)
	   (setf (gethash username *logged-in-users*) object)))


(defun crm-logout (username)
  (remhash username *logged-in-users*))



(defun crm-controller-new-company ()
  (standard-page (:title "Add a new company")
		 (:h1 "Add a new company")
			(:form :action "/company-added" :method "post" 
			       (:p "What is the name of the company?" (:br)
				   (:input :type "text"  
					   :name "name" 
					   :class "txt")
				   (:input :type "text"  
					   :name "address" 
					   :class "txt")

				   )
			       (:p (:input :type "submit" 
					   :value "Add" 
					   :class "btn")))))





(defun crm-controller-company-added ()
  (let  ((cname (hunchentoot:parameter "name"))
	 (caddress (hunchentoot:parameter "address")))
    
    (unless(and  ( or (null cname) (zerop (length cname)))
		 ( or (null caddress) (zerop (length caddress))))
    (new-crm-company cname caddress))
  (hunchentoot:redirect "/crmindex")))


(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-regex-dispatcher "^/crmindex" 'crm-controller-index)
       (hunchentoot:create-regex-dispatcher "^/company-added" 'crm-controller-company-added)
       (hunchentoot:create-regex-dispatcher "^/new-company" 'crm-controller-new-company)
       (hunchentoot:create-regex-dispatcher "^/loginpage" 'crm-controller-loginpage)
       (hunchentoot:create-regex-dispatcher "^/crmlogin" 'crm-controller-login)
       (hunchentoot:create-regex-dispatcher "^/crmlogout" 'crm-controller-logout)))





;(defvar TestAdmin1 (car (clsql:select 'crm-users :where [and [= [slot-value 'crm-users 'username] "TestAdmin1"]
;[= slot-value 'crm-users 'password] "P@ssword1"]]
;				:flatp t)))


;;(defvar TestCompany1 (car (clsql:select 'crm-company :where [= [slot-value 'crm-company 'name] "TestCompany"]
;;				:flatp t)))










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


