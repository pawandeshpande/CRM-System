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



