
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: crm-system.asd,v 1.6 2016/01/26 18:31:03 

;;; Copyright (c) 2015-2016, Pawan Deshpande.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(asdf:defsystem #:crm-system
  :serial t
  :description "CRM System is an accounting application. It specializes in double entry accounting. Backend is MYSQL database and it is a web application."
  :author "Pawan Deshpande <pawan.deshpande@gmail.com>"
  :license "NOT DEFINED YET"
  :depends-on (#:hunchentoot
               #:cl-who
	       #:clsql)
  :components ((:file "packages")
               (:file "crm-ui-sys")
	       (:file "crm-ui-usr")
	       (:file "crm-bl-usr")
	       (:file "crm-dal-usr")
	       (:file "crm-bl-act")
	       (:file "crm-dal-act")
	       (:file "crm-ui-act")
	       (:file "crm-ui-cmp")
	       (:file "crm-dal-cmp")
	       (:file "crm-bl-cmp")
	       (:file "crm-ini-sys")))

