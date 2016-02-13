

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


(defmacro navigation-bar ()
  `(cl-who:with-html-output (*standard-output* nil)
     (:div :class "navbar navbar-default navbar-inverse navbar-fixed-top"  
	   (:ul :class "nav navbar-nav"
		      (:li :class "active" (:a :href "/crmindex" "Home"))
		      (:li  (:a :href "/crmlogout" "Logout"))))))

		 
		 

(defmacro standard-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en" 
	    :lang "en"
	    (:head 
	     (:meta :http-equiv "Content-Type" 
		    :content    "text/html;charset=utf-8")
	     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
	     (:meta :name "description" :content "")
	     (:meta :name "author" :content "")
	     (:link :rel "icon" :href "favicon.ico")
	     (:title ,title )
	  
	     (:link :href "css/style.css" :rel "stylesheet")
	     (:link :href "css/bootstrap.min.css" :rel "stylesheet")
	     (:link :href "css/bootstrap-theme.min.css" :rel "stylesheet")
	     
	     );; Header completes here.
	    (:body 
		   (navigation-bar)
		   (:div :class "container theme-showcase" :role "main" 

			 (:div :id "header"	 ; CRM System header
			      
	 
			       (:table :class "table" 
				       (:tr (:th "Tenant") (:th "Company") (:th "User"))
				       (:tr (:td  :height "12px" (str (get-login-tenant-id)))
					    (:td  :height "12px" (str (get-current-login-company)))
					    (:td  :height "12px" (str (get-current-login-username)))))
		   

			    					 
			       ,@body))	;container div close
		   ;; bootstrap core javascript
		   (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js")
	
		   (:script :src "js/bootstrap.min.js")))))



(defun verify-superadmin ();;"Verifies whether username is superadmin" 
  (if (equal (get-current-login-username) "superadmin") T NIL ))

(defun superadmin-login (company-id)
(if (verify-superadmin )
  (setf ( hunchentoot:session-value :login-company)   (get-tenant-name company-id))))

	    

  


(defun crm-controller-index () 
  (if (is-crm-session-valid?)
      (standard-page (:title "Welcome to CRM World")

	(when (verify-superadmin)(htm (:p "Want to create a new company?" (:a :href "/new-company" "here"))
				      	(:p "List companies?" (:a :href "/list-companies" "here"))))

	(unless (verify-superadmin)
	  (htm 
	(:p "Want to create a new user?" (:a :href "/new-user" "here"))
	(:p "List Users" (:a :href "/list-users" "here"))
	(:p "List Accounts" (:a :href "/list-accounts" "here"))
	(:p "Want to create a new account?" (:a :href "/new-account" "here"))
	(:p "List Journal Entries" (:a :href "/list-journal-entries" "here"))
	(:p "Want to create a new Journal Entry?" (:a :href "/new-journal-entry" "here"))
	)))
	(hunchentoot:redirect "/login")))
  
(setq *logged-in-users* (make-hash-table :test 'equal))


(defun crm-controller-loginpage ()
  (standard-page (:title "Welcome to CRM System")
    (:div :class "row background-image: url(resources/login-background.png);background-color:lightblue;" 
	  (:div :class "col-sm-6 col-md-4 col-md-offset-4"
		(:div :class "account-wall"
		      (:h1 :class "text-center login-title"  "Login to CRM System")
		      (:form :class "form-signin" :role "form" :method "POST" :action "/crmlogin"
			     (:div :class "form-group"
				   (:input :class "form-control" :name "company" :placeholder "Company Name"  :type "text"))
			     (:div :class "form-group"
				   (:input :class "form-control" :name "username" :placeholder "User name" :type "text"))
			     (:div :class "form-group"
				   (:input :class "form-control" :name "password"  :placeholder "Password" :type "password"))
			     (:input :type "submit"  :class "btn btn-primary" :value "Login      ")))))))


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
     (progn (crm-logout (get-current-login-username))
	    (hunchentoot:remove-session *current-user-session*)
	    (hunchentoot:redirect "/login")))


(defun get-current-login-username ()
  (hunchentoot:session-value :login-username))

(defun get-current-login-company ()
 ( hunchentoot:session-value :login-company))

(defun is-crm-session-valid? ()
 (if  (null (get-current-login-username)) NIL T))


(defun crm-login (&key company-name username password)
  (let ((login-user (car (clsql:select 'crm-users :where [and
				       [= [slot-value 'crm-users 'username] username]
				       [= [slot-value 'crm-users 'password] password]
				       [= [slot-value 'crm-users 'tenant-id] (get-tenant-id company-name )]]
				       :flatp t))))

    (if (null login-user) NIL  (progn (add-login-user username  login-user)
				      (setf *current-user-session* (hunchentoot:start-session))
				      (setf (hunchentoot:session-value :login-username) username)
				      (setf (hunchentoot:session-value :login-tenant-id) (get-tenant-id company-name))
				      (setf (hunchentoot:session-value :login-company) company-name)))))


  
  


(defun get-tenant-id (company-name)
  ( car ( clsql:select [row-id] :from [crm-company] :where [= [slot-value 'crm-company 'name] company-name]
		       :flatp t)))

(defun get-tenant-name (company-id)
  ( car ( clsql:select [name] :from [crm-company] :where [= [slot-value 'crm-company 'row-id] company-id]
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

	       (:h3 "Create the Admin user")
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
					 :class "txt")))

					   
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
	  (new-crm-user name username password email (get-login-tenant-id)))
	(hunchentoot:redirect  "/crmindex"))
      (hunchentoot:redirect "/login")))



(defun crm-controller-company-added ()
  (if (is-crm-session-valid?)
      (let  ((cname (hunchentoot:parameter "name"))
	     (caddress (hunchentoot:parameter "address"))
	     (name (hunchentoot:parameter "name"))
	     (username (hunchentoot:parameter "username"))
	     (password (hunchentoot:parameter "password"))
	     (email (hunchentoot:parameter "email")))
    
	(unless(and  ( or (null cname) (zerop (length cname)))
		     ( or (null caddress) (zerop (length caddress)))
		     ( or (null name) (zerop (length name)))
 		      ( or (null username) (zerop (length username)))
		      ( or (null password) (zerop (length password)))
		      ( or (null email) (zerop (length email))))
	  (new-crm-company cname caddress))
	;; By this time the new company is created.
	(let ((company (car (clsql:select 'crm-company :where [= [:name] cname] :caching nil :flatp t))))
	  (new-crm-user name username password email (slot-value company 'row-id)))
	  
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
       (hunchentoot:create-regex-dispatcher "^/deluser" 'crm-controller-delete-user)
       (hunchentoot:create-regex-dispatcher "^/user-added" 'crm-controller-user-added)
       (hunchentoot:create-regex-dispatcher "^/crmlogout" 'crm-controller-logout)
       (hunchentoot:create-regex-dispatcher "^/delcomp" 'crm-controller-delete-company)
 (hunchentoot:create-regex-dispatcher "^/journal-entry-added" 'crm-controller-journal-entry-added)
        (hunchentoot:create-regex-dispatcher "^/account-added" 'crm-controller-account-added)
       (hunchentoot:create-regex-dispatcher "^/new-account" 'crm-controller-new-account)
       (hunchentoot:create-regex-dispatcher "^/delaccount" 'crm-controller-delete-account)
              (hunchentoot:create-regex-dispatcher "^/deljournal-entry" 'crm-controller-delete-journal-entry)
       (hunchentoot:create-regex-dispatcher "^/list-journal-entries" 'crm-controller-list-journal-entries)
        (hunchentoot:create-regex-dispatcher "^/new-journal-entry" 'crm-controller-new-journal-entry)
       (hunchentoot:create-regex-dispatcher "^/list-users" 'crm-controller-list-users)
       (hunchentoot:create-regex-dispatcher "^/list-accounts" 'crm-controller-list-accounts)
       (hunchentoot:create-regex-dispatcher "^/list-companies" 'crm-controller-list-companies)))





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


