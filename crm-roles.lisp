(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

(clsql:def-view-class crm-roles ()
  ((row-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :row-id)
   (name
    :type (string 30)
    :initarg :name)
   (description
    :type (string 255)
    :initarg :address)

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
    :ACCESSOR roles-company
    :DB-KIND :JOIN
    :DB-INFO (:JOIN-CLASS crm-company
	                  :HOME-KEY tenant-id
                          :FOREIGN-KEY row-id
                          :SET NIL))

   
   (parent-id
    :type integer
    :initarg :parent-id)
   (PARENT
    :accessor role-parent
    :db-kind :join
    :db-info (:join-class crm_roles
                          :home-key parent-id
                          :foreign-key row-id
                          :set nil)))


   (:base-table crm_roles))


(defun clone-crm-roles ()
  (let (( *company-roles*  (clsql:select 'crm-roles :where [= [slot-value 'crm-roles 'tenant-id] 1]
					     :flatp t)))
    (loop for role  in *company-roles*
       do ( setf (slot-value role 'tenant-id) (get-login-tenant-id))
	 (clsql:update-records-from-instance role))))




(defun html-select-roles (stream)
    (cl-who:with-html-output (stream)
      (:select (:option :value "pawan")
	       (:option :value "pawan2" :selected "selected" ))))
