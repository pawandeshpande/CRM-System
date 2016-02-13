(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class crm-journal-entry ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
  ; not using currently
;   (opportunity-id
 ;   :type (string 10)
  ;  :db-constraints :not-null
   ; :initarg :opportunity-id)
   (name
    :type (string 30)
    :db-constraints :not-null
    :initarg :name)
   (description
    :type (string 100)
    :initarg :description)

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
    :ACCESSOR journal-entry-company
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-company
	                  :HOME-KEY tenant-id
                          :FOREIGN-KEY row-id
                          :SET NIL))

   
   (deleted-state
    :type (string 1)
    :void-value "N"
    :initarg :deleted-state))
  
  (:base-table crm_opportunity))


(clsql:def-view-class crm-journal-debit-credit-entry ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
   (description
    :type (string 100)
    :initarg :description)
   (account-id
    :TYPE INTEGER
    :INITARG :account-id)
   (journal-account-id
    :ACCESSOR journal-entry-account
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-account
                          :HOME-KEY account-id
                          :FOREIGN-KEY row-id
                          :SET NIL))
     (opty-id
    :TYPE INTEGER
    :INITARG :opty-id)
   (journal-opty-id
    :ACCESSOR journal-entry-opty
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-opportunity
                          :HOME-KEY opty-id
                          :FOREIGN-KEY row-id
                          :SET NIL))
   (created-by
    :TYPE INTEGER
    :INITARG :created-by)
   (user-created-by
    :ACCESSOR journal-entry-created-by
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
    :ACCESSOR journal-entry-company
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-company
	                  :HOME-KEY tenant-id
                          :FOREIGN-KEY row-id
                          :SET NIL)))

  (:base-table crm_acct_opty))




(defun new-crm-journal-entry(name description  tenant-id )
    (clsql:update-records-from-instance (make-instance 'crm-journal-entry
				    :name name
				    :description description
				    :deleted-state "N"
				    :tenant-id tenant-id
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id))))


(defun new-crm-debit-credit-entry (journal-entry-id account-id debit credit)
(clsql:update-records-from-instance (make-instance 'crm-journal-debit-credit-entry
						   :opty-id journal-entry-id
						   :account-id account-id
						   :tenant-id (get-login-tenant-id)
				   :amount (if (> 0 debit) (- debit) credit)
				    :created-by (get-login-tenant-id)
				    :updated-by (get-login-tenant-id))))



(defun list-crm-journal-entries ()
  (clsql:select 'crm-journal-entry  :where [and [= [:deleted-state] "N"] [= [:tenant-id] (get-login-tenant-id)]]   :caching nil :flatp t ))

(defun delete-crm-journal-entry ( id )
  (let ((journal-entry (car (clsql:select 'crm-journal-entry :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value journal-entry 'deleted-state) "Y")
    (clsql:update-record-from-slot journal-entry 'deleted-state)))
    

(defun delete-crm-journal-entries ( list )
  (mapcar (lambda (id)  (let ((journal-entry (car (clsql:select 'crm-journal-entry :where [= [:row-id] id] :flatp t :caching nil))))
			  (setf (slot-value journal-entry 'deleted-state) "Y")
			  (clsql:update-record-from-slot journal-entry  'deleted-state))) list ))


(defun restore-deleted-crm-journal-entries ( list )
(mapcar (lambda (id)  (let ((journal-entry (car (clsql:select 'crm-company :where [= [:row-id] id] :flatp t :caching nil))))
    (setf (slot-value journal-entry 'deleted-state) "N")
    (clsql:update-record-from-slot journal-entry 'deleted-state))) list ))


(defun crm-controller-delete-journal-entry ()
(if (is-crm-session-valid?)
    (let ((id (hunchentoot:parameter "id")) )
      (delete-crm-journal-entry id)
      (hunchentoot:redirect "/list-journal-entries"))
     (hunchentoot:redirect "/login")))


(defun crm-controller-list-journal-entries ()
(if (is-crm-session-valid?)
   (let (( journal-entries (list-crm-journal-entries)))
    (standard-page (:title "List Journal Entries")
      (:table :class "table table-striped" 
	      (:tr (:th "Name") (:th "Description") (:th "Action"))
      (if (= (list-length journal-entries) 0) (htm (:tr (:td  :height "12px" (:p "No journal-entry Found"))))
      (loop for journal-entry in journal-entries
       do (htm (:tr (:td  (str (slot-value journal-entry 'name)))
		    (:td  (str (slot-value journal-entry 'description)))
		    (:td  (:a :href  (format nil  "/deljournal-entry?id=~A" (slot-value journal-entry 'row-id)) "Delete")))))))))
    (hunchentoot:redirect "/login")))



(defun crm-controller-new-journal-entry ()
  (if (is-crm-session-valid?)
      (standard-page (:title "Add a new journal-entry")
	(:h1 "Add a new journal-entry")
	(:form :action "/journal-entry-added" :method "post" 
	       (:p "Name: "
		   (:input :type "text"  :maxlength 30
			   :name "name" 
			   :class "txt")
		   (:p "Description: " (:textarea :rows 4 :cols 50  :maxlength 255   
					    :name "description" 
					    :class "txt"))

		   (:p (journal-entry-page)))))
      (hunchentoot:redirect "/login")))


(defun crm-controller-subsequent-journal-entry (name)
  	;; get the journal entry just created above. 
	(let ((object (clsql:select 'crm-journal-entry :where [= [:name] name] :caching nil :flatp t)))
	  ;; let us create a debit and credit entry for the journal entry

	  
;;	  (new-crm-debit-credit-entry 
))
	   
(defun crm-controller-journal-entry-added ()
  (if (is-crm-session-valid?)
      (let  ((name (hunchentoot:parameter "name"))
	     (description (hunchentoot:parameter "description")))
	     
	(unless(and  ( or (null name) (zerop (length name)))
		     ( or (null description) (zerop (length description)))))
	  
	(new-crm-journal-entry name description (get-login-tenant-id))
	(hunchentoot:redirect  "/crmindex"))
      (hunchentoot:redirect "/login")))


;; This is accounts dropdown
(defmacro accounts-dropdown (dropdown-name)
  `(cl-who:with-html-output (*standard-output* nil)
     (let ((count 0)(accounts (list-crm-accounts 2)))
     (htm (:select :name ',dropdown-name  
      (loop for acct in accounts
	 do (htm  (:option :value (incf count) (str (slot-value acct 'name))))))))))

;; This is a text control with account dropdown and debit and credit text fields.
(defmacro drcr-entry-control (dropdown-name)
  `(cl-who:with-html-output (*standard-output* nil)
     (:p (:label :for ',dropdown-name "Account: ")
	 (let ((count 0)(accounts (list-crm-accounts 2)))
	   (htm (:select :name ',dropdown-name  
			 (loop for acct in accounts
			    do (htm  (:option :value (incf count) (str (slot-value acct 'name)))))))))

     (:p "Debit" (:input :type "text"  
			 :name ',(format nil "Debit-~A" dropdown-name)
			 :class "txt"))
     (:p "Credit" (:input :type "text"  
			  :name ',(format nil "Credit-~A" dropdown-name) 
			  :class "txt"))))




(defmacro journal-entry-details-buttons ()
  `(cl-who:with-html-output (*standard-output* nil)
     (:p (:input :type "submit"
		 
			       :value "Add" 
			       :class "btn")
(:input :type "submit" :value "Finalize" :class "btn")
(:ijput :type "cancel" :value "Cancel" :class "btn"))))





(defmacro journal-entry-page ()
  `(cl-who:with-html-output (*standard-output* nil)
     (:p "Source: " (drcr-entry-control "SourceAccount" ))
     (:p "Destination: " (drcr-entry-control "DestAccount"))
     (:p (journal-entry-details-buttons))))





