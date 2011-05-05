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
  (log-message 'debug "Adding user: ~a" (username user))
  (update-records-from-instance user))

(defmethod delete-user ((user user))
  (log-message 'debug "Deleting user: ~a" (username user))
  (delete-instance-records user))

#.(locally-enable-sql-reader-syntax)
(defun find-user (username)
  (first (select 'user :where [= [username] username]
                 :refresh t :flatp t)))

(defun get-all-users ()
  (select 'user :refresh t :flatp t))

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
;; TODO
;;(defresource :GET "^/users/?$" ()
;;  (print-representation 'users (get-all-users)))

(defresource :POST "^/users/?$" ()
  (let ((user (parse-representation 'user
                                    (post-parameter "input"))))
    (add-user user)
    (setf (return-code*) 201)))

(defresource :GET "^/users/([a-zA-Z0-9]+)/?$" (username)
  (let ((user (find-user username)))
    (when (not user)
      (error 'user-dont-exists))
    (print-representation 'user user)))

(defresource :UPDATE "^/users/([a-zA-Z0-9]+)/?$" (username)
  (let ((new-user (parse-representation 'user
                                        (post-parameter "input")))
        (old-user (find-user username)))
    (when (not old-user)
      (error 'user-dont-exists))
    (update-records-from-instance (merge-instances old-user new-user))
    (setf (return-code*) 201)))

(defresource :DELETE "^/users/([a-zA-Z0-9]+)/?$" (username)
  (let ((user (find-user username)))
    (when (not user)
      (error 'user-dont-exists))
    (delete-user user)
    (setf (return-code*) 204)))