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

(defpage register "Register"
  (if (and (parameter "username") (> (length (parameter "username")) 2)
           (parameter "password") (> (length (parameter "password")) 5))
      (htm
       (add-user (parameter "username") (hash-password (parameter "password"))))
      (htm
       (:form :action "register" :method "post"
              (:p "Username: " (:input :type "text" :name "username"))
              (:p "Password: " (:input :type "password" :name "password"))
              (:p (:input :type "submit" :value "Register"))))))

(defun add-user (username password)
  (let ((user (make-instance 'user :username username :password password)))
    (update-records-from-instance user)))