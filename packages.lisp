(in-package :cl-user)
(defpackage :com.desh.crm-system
  (:use :cl :cl-who :hunchentoot :clsql)
  (:nicknames :crm-system)
  (:export #:*logged-in-users*
	    #:*http-server*))

