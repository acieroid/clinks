(in-package :clinks)

(def-view-class link ()
  ((id :type integer :db-kind :key :initform nil
       :reader id :initarg :id)
   (user-id :type integer
            :initarg :user-id)
   (user :db-kind :join :db-info (:join-class user :home-key user-id :foreign-key id :set nil)
         :accessor user)
   (url :type string
        :accessor url :initarg :url)
   (tags :type string
         :accessor tags :initarg :tags)))

(defmethod print-html ((link link))
  (with-html-output-to-string (stream)
    (:div :class "link"
          (:a :href (url link)
              (str (url link)))
          :br)))

(defun all-links ()
  (select 'link :flatp t :refresh t))

(defun add-link (url tags user)
  (let ((link (make-instance 'link
                             :url url :tags tags :user user)))
    (update-records-from-instance link)))

(defpage links "All links"
  (:ul
   (mapcar (lambda (x)
             (htm (:li (str (print-html x)))))
           (all-links))))

(defpage new-link "New link"
  (if (and (parameter "url") (not (string= (parameter "url") "")))
      (htm
       (add-link (parameter "url") (parameter "tags") (session-value :user))
       (:p "Link added"))
      (htm
       (:form :action "new-link" :method "post"
              (:p "URL:" (:input :type "text" :name "url"))
              (:p "Tags:" (:input :type "text" :name "tags"))
              (:p (:input :type "submit" :value "Add"))))))

