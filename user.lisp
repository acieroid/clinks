(in-package :clinks)

(def-view-class user (data)
  ((username :type string :accessor username :initarg :username)
   (password :type string :accessor password :initarg :password)))

(defun make-user (username password)
  (make-instance 'user
                 :username username
                 :password (hash password)))

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
  (xml (<> 'user
           (<> 'username (username user))
           (<> 'time (rfc3339 user))
           (print-representation 'links (get-user-links user)))))

(defmethod parse-representation ((type (eql 'user)) string)
  (parse-fields `((username "^[a-zA-Z0-9]+$" ,#'identity)
                  (password "^[a-zA-Z0-9]+$" ,#'hash))
                'user
                string))
