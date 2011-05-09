(in-package :clinks)

(def-view-class link (data)
  ((user-id :type integer :accessor user-id :initarg :user-id)
   (user :db-kind :join :accessor user
         :db-info (:join-class user :home-key user-id
                   :foreign-key id :set nil))
   (url :type string :accessor url :initarg :url)
   (title :type string :accessor title :initarg :title)
   (notes :type string :accessor notes :initarg :notes)
   (tags :type string :accessor tag-string :initarg :tag-string)))

(defun make-link (user url title notes tags)
  (make-instance 'link
                 :user-id (id user)
                 :url url :title title :notes notes
                 :timestamp (now)
                 :tag-string tags ;; is TAGS a list or a string ?
                 ))

(defmethod get-href ((link link))
  (format nil "/users/~a/links/~a" (username (user link)) (url link)))

;;; Representation
(defmethod print-representation ((type (eql 'link)) link)
  (xml (<> 'link
           (<> 'url (url link))
           (<> 'time (rfc3339 link))
           (<> 'title (title link))
           (<> 'notes (notes link))
           (print-representation 'tags (tag-string link)))))

(defmethod print-representation ((type (eql 'links)) links)
  (xml (<> 'links
           (<>iter (lambda (link)
                     (<> 'link :href (get-href link)
                         (<> 'url (url link))))
                   links))))

;;; Resources
(defresource :GET "^/users/([a-zA-Z0-9]+)/links/?$" (username)
  (let ((user (find-user username)))
    (when (not user)
      (error 'user-dont-exists :username username))
    (print-representation 'links (get-links user))))
