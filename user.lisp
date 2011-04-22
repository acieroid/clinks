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

(defmethod parse-representation ((type (eql 'user)) xml)
  (let ((user (make-instance 'user))
        (current-element nil))
    (flet ((new-element (name attributes seed)
             (declare (ignore attributes))
             ;; Avoid case problems
             (setf current-element (intern (string-upcase (symbol-name name))))
             seed)
           (text (string seed)
             (case current-element
               ;; TODO: check if the slot is already bound and check
               ;; if strings don't use forbidden characters
               (username (setf (username user) string))
               (password (setf (password user) (hash string)))
               (otherwise (error 'unknown-field :field current-element)))
             seed))
      (with-input-from-string (stream xml)
        (s-xml:start-parse-xml stream
                               (make-instance 's-xml:xml-parser-state
                                              :new-element-hook #'new-element
                                              :text-hook #'text))))
    user))