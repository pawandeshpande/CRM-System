(asdf:defsystem #:crm-system
  :serial t
  :description "CRM System is an accounting application. It specializes in double entry accounting. Backend is MYSQL database and it is a web application."
  :author "Pawan Deshpande <pawan.deshpande@gmail.com>"
  :license "NOT DEFINED YET"
  :depends-on (#:hunchentoot
               #:cl-who
	       #:clsql)
  :components ((:file "packages")
               (:file "crm-system")
	       (:file "crm-users")
	       (:file "crm-company")
	       (:file "crm-accounts")
	       (:file "crm-system-init")))
