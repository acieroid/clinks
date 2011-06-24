(in-package :clinks)

(define-condition clinks-error ()
  ((code :initarg :code :reader code :initform 500)
   (reason :initarg :reason :reader reason))
  (:documentation "Parent class of every clinks related error"))

(defmethod print-object :before ((e clinks-error) stream)
  (princ "<error>" stream))

(defmethod print-object :after ((e clinks-error) stream)
  (princ "</error>" stream))

;;; Parse errors
(define-condition parse-representation-error (clinks-error)
  ((code :initform 415)
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
   (username :initarg :username :reader username))
  (:documentation "Error related with user manipulation"))

(defmethod print-object ((e user-error) stream)
  (format stream "Error with user '~a': ~a" (username e) (reason e)))

(define-condition user-doesnt-exists (user-error)
  ((reason :initform "User doesn't exists")))

(define-condition user-already-exists (user-error)
  ((reason :initform "User already exists")))

(define-condition wrong-password (user-error)
  ((reason :initform "Wrong password")))

(define-condition not-your-user (user-error)
  ((reason :initform "Not your user")))

(define-condition not-logged (user-error)
  ((reason :initform "Not logged")
   (username :initform "")))

(defmethod print-object ((e not-logged) stream)
  (format stream "~a" (reason e)))

;;; Link manipulation errors
(define-condition link-error (clinks-error)
  ((code :initform 404)
   (url :initarg :url :reader url)))

(defmethod print-object ((e link-error) stream)
  (format stream "Error with link '~a': ~a" (url e) (reason e)))