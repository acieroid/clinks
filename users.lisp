(in-package :clinks)

(def-view-class user ()
  ((id :type integer :db-kind :key :initform nil
       :reader id :initarg :id)
   (username :type string
             :accessor username :initarg :username)
   (password :type string
             :accessor password :initarg :password)))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
                             (ironclad:ascii-string-to-byte-array password))))

#.(locally-enable-sql-reader-syntax)
(defun find-user (username)
  (first (select 'user :where [= [username] username] :refresh t :flatp t)))
#.(restore-sql-reader-syntax-state)

(defun current-user ()
  (find-user (session-value :user)))

(defun add-user (username password)
  (when (find-user username)
    (error "User already exists"))
  (let ((user (make-instance 'user :username username :password password)))
    (update-records-from-instance user)))

(defun right-user-ids-p (username password)
  (let ((user (find-user username)))
    (and user
         (string= (password user) password))))

(defun connect-user (username password)
  (if (right-user-ids-p username password)
      (progn
        (start-session)
        (setf (session-value :user) username))
      ;; TODO: use conditions and an error page?
      (error "No such user or wrong password")))

(defun user-form (page name)
  (with-html-output-to-string (stream)
    (:form :action page :method "post"
           (:p "Username: " (:input :type "text" :name "username"))
           (:p "Password: " (:input :type "password" :name "password"))
           (:p (:input :type "submit" :value (str name))))))

(defpage register "Register"
  (if (and (parameter "username") (> (length (parameter "username")) 2)
           (parameter "password") (> (length (parameter "password")) 5))
      (progn
        (add-user (parameter "username") (hash-password (parameter "password")))
        (str "Registration successful"))
      (str (user-form "register" "Register!"))))

(defpage connect "Connect"
  (if (and (parameter "username") (parameter "password"))
      (progn
        (connect-user (parameter "username") (hash-password (parameter "password")))
        (str "Connection successful"))
      (str (user-form "connect" "Connect!"))))

(defpage disconnect "Disconnect"
  (remove-session *session*))