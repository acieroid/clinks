(in-package :clinks)

(def-view-class link (data)
  ((user-id :type integer :accessor user-id :initarg :user-id)
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

;;;; Representation
(defmethod print-representation ((type (eql 'link)) link)
  (<< 'link
   (<< 'url (url link))
   (<< 'time (rfc3339 link))
   (<< 'title (title link))
   (<< 'notes (notes link))
   (print-representation 'tags (tag-string link))))

(defmethod print-representation ((type (eql 'links)) links)
  (<< 'links
   (<<iter
    (lambda (link)
      (<< 'link
          :href (get-href link)
          (<< 'url (url link))))
    links)))

