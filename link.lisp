(in-package :clinks)

(def-view-class link (data)
  ((user-id :type integer :accessor user-id :initarg :user-id)
   (user :db-kind :join :accessor user
         :db-info (:join-class user :home-key user-id
                   :foreign-key id :set nil))
   (url :type string :accessor url :initarg :url :initform "")
   (title :type string :accessor title :initarg :title :initform "")
   (notes :type string :accessor notes :initarg :notes :initform "")
   (tags :type string :accessor tag-string :initarg :tag-string :initform "")))

(defmethod get-href ((link link))
  (format nil "/users/~a/links/~a" (username (user link)) (url link)))

;;; Database manipulation
(defmethod add-link ((link link))
  (log-message 'info "Adding link: ~a" (url link))
  (update-records-from-instance link))

(defmethod delete-link ((link link))
  (log-message 'info "Deleting link: ~a" (url link))
  (delete-instance-records link))

;;; Representation
(defmethod print-representation ((type (eql 'link)) link)
  (xml (<> 'link
           (<> 'url (url link))
           (<> 'timestamp (rfc3339 link))
           (<> 'title (title link))
           (<> 'notes (notes link))
           ;; TODO
;           (print-representation 'tags (tag-string link)
           (<> 'tags (tag-string link)))))

(defmethod print-representation ((type (eql 'links)) links)
  (xml (<> 'links
           (<>iter (lambda (link)
                     (<> 'link :href (get-href link)
                         (<> 'url (url link))
                         (<> 'timestamp (rfc3339 link))
                         (<> 'title (title link))
                         (<> 'notes (notes link))))
                   links))))

(defmethod parse-representation ((type (eql 'link)) string)
  (parse-fields `((url "^<url>$" ,#'identity)
                  (title "^<title>$" ,#'identity)
                  (notes "^<notes>$" ,#'identity)
                  (tags "^<tags>$" ,#'identity))
                'link
                string))

;;; Resources
(defresource :GET "^/users/(<username>)/links/?$" (username)
  (let ((user (find-user username)))
    (unless user
      (error 'user-doesnt-exists :username username))
    (print-representation 'links (get-links user))))

(defresource :GET "^/users/(<username>)/links/(<url>)$" (username url)
  (let ((user (find-user username)))
    (unless user
      (error 'user-doesnt-exists :username username))
    (let ((link (find-link user url)))
      (unless link
        (error 'link-doesnt-exists :url url))
      (print-representation 'link link))))

(defresource-logged user :POST "/links/?$" ()
  (let ((link (parse-representation 'link
                                    (post-parameter "input"))))
    (setf (user-id link) (id user))
    (add-link link)
    (setf (return-code*) 201)))

(defresource-logged user :POST "/links/(<url>)$" (url)
  (let ((link (find-link user url))
        (new-link (parse-representation 'link
                                        (post-parameter "input"))))
    (unless link
      (error 'link-doesnt-exists :url url))
    (update-records-from-instance (merge-instances new-link link))
    (setf (return-code*) 201)))

(defresource-logged user :DELETE "/links/(<url>)$" (url)
  (let ((link (find-link user url)))
    (unless link
      (error 'link-doesnt-exists :url url))
    (delete-link link)
    (setf (return-code*) 204)))