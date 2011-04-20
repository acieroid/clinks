(in-package :clinks)

(def-view-class user ()
  ((id :type integer :db-kind :key :initform nil :reader id)
   (username :type string :accessor username :initform "" :initarg :username)
   (password :type string :accessor password :initform "" :initarg :password)
   (timestamp :type integer :accessor timestamp :initform 0 :initarg :timestamp)))

(defun make-user (username password)
  (make-instance 'user
                 :username username
                 :password (hash password)
                 :timestamp (now)))

;;; Conditions
(define-condition user-error ()
  ((username :initarg :username :reader username)
   (reason :initarg :reason :reader reason)))

(define-condition user-dont-exists (user-error)
  ((reason :initform "User don't exists")))

(define-condition user-already-exists (user-error)
  ((reason :initform "User already exists")))

;;; Database manipulation
#.(locally-enable-sql-reader-syntax)
(defun find-user (username)
  (first (select 'user :where [= [username] username]
                 :refresh t :flatp t)))

(defun get-user-links (user)
  nil)

(defun add-user (username password)
  (when (find-user username)
    (error 'user-already-exists :username username))
  (update-records-from-instance (make-user username password)))
#.(restore-sql-reader-syntax-state)

;;; Representations
(defmethod print-representation ((type (eql 'user)) user)
  (<< 'user
   (<< 'username (username user))
   (<< 'time (rfc3339 user))
   (print-representation 'links (get-user-links user))))

