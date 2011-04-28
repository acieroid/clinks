(in-package :clinks)

(def-view-class user (data)
  ((username :type string :accessor username :initarg :username)
   (password :type string :accessor password :initarg :password)))

(defun make-user (username password)
  (make-instance 'user
                 :username username
                 :password (hash password)))

;;; Database manipulation
(defmethod add-user ((user user))
  (when (find-user (username user))
    (error 'user-already-exists :username (username user)))
  (update-records-from-instance user))

#.(locally-enable-sql-reader-syntax)
(defun find-user (username)
  (first (select 'user :where [= [username] username]
                 :refresh t :flatp t)))

(defun get-user-links (user)
  nil)
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

;;; Resources
(defresource :POST "^/users/?$" ()
  (handler-case
      (let ((user (parse-representation 'user
                                        (post-parameter "input"))))
        (add-user user))
    ;; TODO: move error handling somewhere else or use hunchentoot's
    ;; facilities to handle errors
    (parse-representation-error (e)
      (setf (return-code*) 415)
      (format nil "Parse error on field ~a: ~a" (field e) (reason e)))
    (user-already-exists (e)
      (setf (return-code*) 401)
      (format nil "User already exists: ~a" (username e)))))