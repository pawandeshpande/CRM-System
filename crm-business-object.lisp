(in-package :crm-system)
(clsql:file-enable-sql-reader-syntax)

(defclass crm-bo-definition ()
  ((name
    :accessor name
    :initarg :name
    :initform (error "Must supply Business Object name")
    :documentation "Business object name.")
   
   (business-component-list
    :accessor business-component-list
    :initarg :business-component-list
    :initform (error "Must have a list of Business components")
    :documentation "Business component list associated with this business object")))

(defclass crm-bc-definition ()
  ((name
    :accessor name
    :initarg :name
    :initform (error "Must supply Business component name")
    :documentation "Business component name.")
   (persistance-class
    :accessor persistance-class
    :initarg :persistance-class
    :initform (error "Must supply a persistance class, defined using clsql:def-view-class")
    :documentation "The persistance class used in this implementation is defined using CLSQL:DEF-VIEW-CLASS")
    (can-delete?
    :accessor can-delete?
    :initarg :can-delete?
    :initform (error "Must specify whether this business component can be deleted by a user")
    :documentation "A boolean TRUE/NIL value, which specifies whether this business component can be deleted by a user of the CRM system.") ))


(defgeneric delete-business-component (bus-comp-instance)
  ( :documentation "Delete the business component"))



