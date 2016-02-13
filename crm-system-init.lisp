
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


;; Connect to the database (see the CLSQL documentation for vendor
;; specific connection specs).

(defun crm-db-connect (&key strdb strusr strpwd servername strdbtype)
  :documentation "This function is responsibile for connecting to the CRM system. Arguments accepted are 
Database 
Username
Password 
Servername 
Database type: Supported type is ':odbc'"

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

    (clsql:start-sql-recording)))



(defvar *http-server* nil)

(defun start-crm-system () 
  (setf  *http-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242 :document-root #p"~/crm-system/")))
(setf (hunchentoot:acceptor-access-log-destination *http-server* ) #p"~/hunchentoot-access.log")
(setf (hunchentoot:acceptor-message-log-destination *http-server*) #p"~/hunchentoot-messages.log")
(crm-db-connect :strdb "TestCRMCore" :strusr "TestCRMCore" :strpwd "TestCRMCore" :strdbtype :odbc)
(defparameter *crm-account-types* (clsql:select [:name] :from 'crm-account-type :caching nil :flatp t)))

(defun shutdown-crm-system ()
  (format t "******** Stopping SQL Recording *******~C"  #\linefeed)
  (clsql:stop-sql-recording :type :both)
  (format t "******** DB Disconnect ********~C" #\linefeed)
  (clsql:disconnect)
  (format t "******* Stopping HTTP Server *********~C"  #\linefeed)
  (hunchentoot:stop *http-server*)
)


