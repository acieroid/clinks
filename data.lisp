(in-package :clinks)

;;; Conditions
(define-condition parse-representation-error ()
  ((reason :initarg :reason :reader reason)
   (field :initarg :field :reader field)))

(define-condition unknown-field (parse-representation-error)
  ((reason :initform "Unknown field")))

(define-condition forbidden-characters (parse-representation-error)
  ((reason :initform "Forbidden characters")))

(define-condition user-error ()
  ((username :initarg :username :reader username)
   (reason :initarg :reason :reader reason)))

(define-condition user-dont-exists (user-error)
  ((reason :initform "User don't exists")))

(define-condition user-already-exists (user-error)
  ((reason :initform "User already exists")))

;;; Methods
(defgeneric print-representation (type object)
  (:documentation "Print the XML representation of an object"))

(defgeneric parse-representation (type xml)
  (:documentation "Parse a XML representation of an object and return the object created"))

(defgeneric rfc3339 (object)
  (:documentation "Return a string containing the creation date of the object")
  (:method (object)
    (local-time:format-rfc3339-timestring
     nil
     (local-time:unix-to-timestamp (timestamp object)))))

(defgeneric id (object)
  (:documentation "Return the id of OBJECT"))

(defgeneric get-href (object)
  (:documentation "Return the link to OBJECT's representation")
  (:method (object)
    (format nil "~a/~a" (class-name (class-of object)) (id object))))

;;; Parent class of all data's classes
(def-view-class data ()
  ((id :type integer :db-kind :key :initform nil :reader id)
   (timestamp :type integer :accessor timestamp :initform 0)))

(defmethod initialize-instance :after ((data data) &key &allow-other-keys)
  (setf (timestamp data) (now)))