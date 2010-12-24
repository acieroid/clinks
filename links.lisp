(in-package :clinks)

(def-view-class link ()
  ((id :type integer :db-kind :key :initform nil
       :reader id :initarg :id)
   (url :type string
        :accessor url :initarg :url)
   (tags :type string
         :accessor tags :initarg :tags)))

(defmethod print-html ((link link))
  (with-html-output-to-string (stream)
      (htm (:a :href (url link)
               (str (url link)))
           :br)))

(defpage links "All links"
  (:ul
   (mapcar (lambda (x)
             (htm (:li (str (print-html x)))))
           (all-links))))

(defpage new-link "New link"
  (if (and (parameter "url") (not (string= (parameter "url") "")))
      (htm
       (add-link (parameter "url") (parameter "tags"))
       (:p "Link added"))
      (htm
       (:form :action "new-link" :method "post"
              (:p "URL:" (:input :type "text" :name "url"))
              (:p "Tags:" (:input :type "text" :name "tags"))
              (:p (:input :type "submit" :value "Add"))))))

(defun all-links ()
  (select 'link :flatp t :refresh t))

#.(locally-enable-sql-reader-syntax)
(defun get-max-id ()
  (reduce (lambda (last x)
            (max last x))
          (select [id] :from [link] :flatp t)
          :initial-value 0))
#.(restore-sql-reader-syntax-state)

(let (id)
  (defun new-id ()
    (unless id
      (setf id (get-max-id)))
    (incf id)))

(defun add-link (url tags)
  (let ((link (make-instance 'link
                             :url url :tags tags)))
    (update-records-from-instance link)))