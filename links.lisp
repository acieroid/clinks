(in-package :clinks)

(def-view-class link ()
  ((id :type integer :db-kind :key :initform nil
       :reader id)
   (user-id :type integer
            :initarg :user-id)
   (url :type string
        :accessor url :initarg :url)
   ;; Joins
   (user :db-kind :join :db-info (:join-class user :home-key user-id :foreign-key id :set nil)
         :accessor user)
   (tags :db-kind :join :db-info (:join-class tag :home-key id :foreign-key link-id :set t)
         :accessor tags)))

(defun delete-url (link)
  (concatenate 'string "delete/" (url link)))

(defmethod print-html ((link link))
  (with-html-output-to-string (stream)
    (:div :class "link"
          (:a :href (url link)
              (str (url link)))
          " "
          (:a :href (delete-url link)
              "x")
          :br
          (:div :class "tags"
                (mapcar (lambda (tag) (htm (str (print-html tag)) " "))
                      (tags link))))))

(defun all-links ()
  (select 'link :flatp t :refresh t))

#.(locally-enable-sql-reader-syntax)

(defun add-link (url tags user)
  (let ((link (make-instance 'link :url url :user-id (id user))))
    (update-records-from-instance link)
    ;; The id is set in the db, not in our object
    (let ((id (first (select [max [id]] :from 'link :flatp t :refresh t))))
      (mapcar (curry #'create-tag id) tags))))

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

(defpagel links "My links"
  (:ul
   (mapcar (lambda (x)
             (htm (:li (str (print-html x)))))
           (user-links (current-user)))))

(defpagel new-link "New link"
  (if (and (parameter "url") (not (string= (parameter "url") "")))
      (htm
       (add-link (parameter "url") (split-tags (parameter "tags")) (current-user))
       (:p "Link added"))
      (htm
       (:form :action "new-link" :method "post"
              (:p "URL:" (:input :type "text" :name "url"))
              (:p "Tags:" (:input :type "text" :name "tags") " (space delimited)")
              (:p (:input :type "submit" :value "Add"))))))

