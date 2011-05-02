(in-package :clinks)

(define-condition clinks-error ()
  ((code :initarg :code :reader code :initform 500))
  (:documentation "Parent class of every clinks related error"))

;;; Parse errors
(define-condition parse-representation-error (clinks-error)
  ((code :initform 415)
   (reason :initarg :reason :reader reason)
   (field :initarg :field :reader field))
  (:documentation "Error when parsing the representation of an object"))

(defmethod print-object ((e parse-representation-error) stream)
  (format stream "Parse error on field '~a': ~a" (field e) (reason e)))

(define-condition unknown-field (parse-representation-error)
  ((reason :initform "Unknown field")))

(define-condition forbidden-characters (parse-representation-error)
  ((reason :initform "Forbidden characters")))

;;; User manipulation errors
(define-condition user-error (clinks-error)
  ((code :initform 401)
   (username :initarg :username :reader username)
   (reason :initarg :reason :reader reason))
  (:documentation "Error related with user manipulation"))

(defmethod print-object ((e user-error) stream)
  (format stream "Error with user '~a': ~a" (username e) (reason e)))

(define-condition user-dont-exists (user-error)
  ((reason :initform "User don't exists")))

(define-condition user-already-exists (user-error)
  ((reason :initform "User already exists")))

