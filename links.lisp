(in-package :clinks)

(def-view-class link ()
  ((id :type integer :db-kind :key :initform nil
       :reader id)
   (user-id :type integer :initarg :user-id)
   (url :type string :accessor url :initform "" :initarg :url)
   (title :type string :accessor title :initform "" :initarg :title)
   (notes :type string :accessor notes :initform "" :initarg :notes)
   (private :type boolean :accessor private :initform nil :initarg :private)
   ;; Joins
   (user :db-kind :join :db-info (:join-class user :home-key user-id
                                  :foreign-key id :set nil)
         :accessor user)
   (tags :db-kind :join :db-info (:join-class tag :home-key id
                                  :foreign-key link-id :set t)
         :accessor tags)))

(defmethod print-html ((link link))
  (with-html-output-to-string (stream)
    (:div :class "link"
          (:a :href (url link)
              (str (title link)))
          " "
          (:a :href (get-action-url "edit" (url link)) "edit") " "
          (:a :href (get-action-url "delete" (url link))  "x")
          :br
          (:div :class "tags"
                (mapcar (lambda (tag) (htm (str (print-html tag)) " "))
                      (tags link)))
          (:div :class "notes"
                (str (notes link))))))

(defun all-links ()
  (select 'link :flatp t :refresh t))

#.(locally-enable-sql-reader-syntax)

(defun add-link (user url &optional (title "") (tags nil) (notes "") (private nil))
  (let ((link (make-instance 'link :url url
                             :title title :notes notes
                             :private private
                             :user-id (id user))))
    (update-records-from-instance link)
    ;; The id is set in the db, not in our object
    (let ((id (first (select [max [id]] :from 'link :flatp t :refresh t))))
      (mapcar (curry #'create-tag id) tags))))

(defun edit-link (link url title tags notes private)
  (setf (url link) url
        (title link) title
        (notes link) notes
        (private link) private)
  (delete-records :from [tag] :where [= [link-id] (id link)])
  (mapcar (curry #'create-tag (id link)) tags))

(defun find-link (url user)
  (first (select 'link :where [and [= [user-id] (id user)]
                                   [= [url] url]]
                 :refresh t :flatp t)))

(defun user-links (user)
  (select 'link :where [= [user-id] (id user)]
          :refresh t :flatp t))

#.(disable-sql-reader-syntax)

(defun delete-link (url user)
  (let ((link (find-link url user)))
    (unless link
      (error "No such link ~a" url))
    (let ((tags (tags link)))
      (delete-instance-records link)
      (mapcar #'delete-instance-records tags))))

(defun get-page-title (url)
  (multiple-value-bind (string results)
      (ppcre:scan-to-strings "<title>\\s*([^\\s]*)\\s*</title>"
                             (drakma:http-request url))
    (declare (ignore string))
    (aref results 0)))

(defun link-form (page name &optional (link (make-instance 'link)))
  (with-html-output-to-string (stream)
    (:form :action page :method "post"
           (:p "URL:" (:input :type "text" :name "url" :value (url link)))
           (:p "Title: " (:input :type "text" :name "title" :value (title link))
               " (will be filled automatically)")
           (:p "Tags:" (:input :type "text" :name "tags"
                               :value (format nil "~{~a ~}"
                                              (mapcar #'tag-name (tags link))))
               " (space delimited)")
           (:p "Notes: " (:textarea :name "notes" :value (notes link)))
           (:p "Private: " (:input :type "checkbox" :name "private"
                                   :checked (if (private link) "yes" "no")))
           (:p (:input :type "submit" :value name)))))

(defpagel links "My links"
  (:ul
   (mapcar (lambda (x)
             (htm (:li (str (print-html x)))))
           (user-links (current-user)))))

(defpagel new-link "New link"
  (if (and (parameter "url") (not (string= (parameter "url") "")))
      (htm
       (add-link (current-user) (parameter "url")
                 (if (string= (parameter "title") "")
                     (get-page-title (parameter "url"))
                     (parameter "title"))
                 (split-tags (parameter "tags"))
                 (parameter "notes")
                 (string= (parameter "private") "on"))
       (:p "Link added"))
      (str (link-form "new-link" "Add"))))

(defaction edit "Edit" url
  (let ((link (find-link url (current-user))))
    (if (and (parameter "url") (not (string= (parameter "url") "")))
        (htm
         (edit-link link (parameter "url") (parameter "title")
                    (split-tags (parameter "tags"))
                    (parameter "notes")
                    (string= (parameter "private") "on"))
         "Link edited")
        (str (link-form (request-uri*) "Edit" link)))))

(defaction delete "Delete" url
  (delete-link url (current-user))
  "Link deleted")