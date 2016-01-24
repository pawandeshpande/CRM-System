

(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)


(defvar *logged-in-users* nil)
(defvar *current-user-session* nil)


(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

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
	    (:body :align "center"
	     (:div :id "header"		; CRM System header
		   (:img :src "./resources/crm-logo.png" 
			 :alt "CRM" 
			 :class "logo")
		    (:h3 (:span :class "strapline"  "Welcome  " (str (get-current-login-user))
				)))
	     (:p (:hr))
					 
	     ,@body))))






(defun crm-controller-index () 
  (if (is-crm-session-valid?)
      (standard-page (:title "Welcome to CRM World")
	(:p "Want to create a new company?" (:a :href "/new-company" "here"))
	(:p "Want to create a new user?" (:a :href "/new-user" "here"))
	(:p "Want to create a new account?" (:a :href "/new-account" "here"))
	(:a :href "/crmlogout" "Logout"))
      (hunchentoot:redirect "/login")))
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
		       :value "Login" 
		       :class "btn")))))


(defun crm-controller-login ()
  (let  ((uname (hunchentoot:parameter "username"))
	 (passwd (hunchentoot:parameter "password"))
	 (cname (hunchentoot:parameter "company")))
    
    (unless(and
	    ( or (null cname) (zerop (length cname)))
	    ( or (null uname) (zerop (length uname)))
	    ( or (null passwd) (zerop (length passwd))))
      (if (equal (crm-login :company-name cname :username uname :password passwd) NIL) (hunchentoot:redirect "/login") (hunchentoot:redirect  "/crmindex")))))

  
   (defun crm-controller-logout ()
     (if (is-crm-session-valid?)
     (progn (crm-logout (get-current-login-user))
	    (hunchentoot:remove-session *current-user-session*)
	    (hunchentoot:redirect "/login"))))


(defun get-current-login-user ()
  (hunchentoot:session-value :login-username))

(defun get-current-login-company ()
 ( hunchentoot:session-value :login-company))

(defun is-crm-session-valid? ()
 (if  (null (get-current-login-user)) NIL T))


(defun crm-login (&key company-name username password)
  (let ((login-user (car (clsql:select 'crm-users :where [and
				       [= [slot-value 'crm-users 'username] username]
				       [= [slot-value 'crm-users 'password] password]
				       [= [slot-value 'crm-users 'tenant-id] (get-tenant-id company-name )]]
				       :flatp t))))

    (if (null login-user) NIL  (progn (add-login-user username  login-user)
				      (setf *current-user-session* (hunchentoot:start-session))
				      (setf (hunchentoot:session-value :login-username) username)
				      (setf (hunchentoot:session-value :login-company) company-name)))))


(defun get-tenant-id (company-name)
  ( car ( clsql:select [row-id] :from [crm-company] :where [= [slot-value 'crm-company 'name] company-name]
		       :flatp t)))

      
(defun get-login-user-object (username)
  (gethash username *logged-in-users*))


(defun is-user-already-login? (username)
(if (equal (gethash username *logged-in-users*) NIL ) NIL T))


(defun add-login-user(username object)
  (unless (is-user-already-login? username)
	   (setf (gethash username *logged-in-users*) object)))


(defun crm-logout (username)
  (remhash username *logged-in-users*))



(defun crm-controller-new-company ()
  (if (is-crm-session-valid?)
      (standard-page (:title "Add a new company")
	(:h1 "Add a new company")
	(:form :action "/company-added" :method "post" 
	       (:p "Name: " 
		   (:input :type "text"  
			   :name "name" 
			   :class "txt"))
	       (:p "Address: " (:input :type "textarea"  
				       :name "address" 
				       :class "txtarea"))

				   
	       (:p (:input :type "submit" 
			   :value "Add" 
			   :class "btn"))))
      (hunchentoot:redirect "/login")))


(defun crm-controller-new-user ()
  (if (is-crm-session-valid?)
      (standard-page (:title "Add a new User")
	(:h1 "Add a new User")
	(:form :action "/user-added" :method "post" 
	       (:p "Name: "
		   (:input :type "text"  
			   :name "name" 
			   :class "txt")
		   (:p "Username: " (:input :type "text"  
					    :name "username" 
					    :class "txt"))
		   (:p "Password: " (:input :type "password"  
					    :name "password" 
					    :class "password"))
		   (:p "Email: " (:input :type "text"  
					 :name "email" 
					 :class "txt"))

		   ;; Add a drop down list of available roles for the user.
				  
				  
		   (:p (:input :type "submit" 
			       :value "Add" 
			       :class "btn")))))
      (hunchentoot:redirect "/login")))


(defun crm-controller-user-added ()
  (if (is-crm-session-valid?)
      (let  ((name (hunchentoot:parameter "name"))
	     (username (hunchentoot:parameter "username"))
	     (password (hunchentoot:parameter "password"))
	     (email (hunchentoot:parameter "email")))
    
	(unless (and  ( or (null name) (zerop (length name)))
		      ( or (null username) (zerop (length username)))
		      ( or (null password) (zerop (length password)))
		      ( or (null email) (zerop (length email))))		
	  (new-crm-user name username password email))
	(hunchentoot:redirect  "/crmindex"))
      (hunchentoot:redirect "/login")))



(defun crm-controller-company-added ()
  (if (is-crm-session-valid?)
      (let  ((cname (hunchentoot:parameter "name"))
	     (caddress (hunchentoot:parameter "address")))
    
	(unless(and  ( or (null cname) (zerop (length cname)))
		     ( or (null caddress) (zerop (length caddress))))
	  (new-crm-company cname caddress))
	(hunchentoot:redirect  "/crmindex"))
      (hunchentoot:redirect "/login")))


(setq hunchentoot:*dispatch-table*
      (list
       (hunchentoot:create-regex-dispatcher "^/crmindex" 'crm-controller-index)
       (hunchentoot:create-regex-dispatcher "^/company-added" 'crm-controller-company-added)
       (hunchentoot:create-regex-dispatcher "^/new-company" 'crm-controller-new-company)
       (hunchentoot:create-regex-dispatcher "^/login" 'crm-controller-loginpage)
       (hunchentoot:create-regex-dispatcher "^/crmlogin" 'crm-controller-login)
       (hunchentoot:create-regex-dispatcher "^/new-user" 'crm-controller-new-user)
       (hunchentoot:create-regex-dispatcher "^/user-added" 'crm-controller-user-added)
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


