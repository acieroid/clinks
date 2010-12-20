(in-package :clinks)

(def-view-class link ()
  ((id :type integer :db-kind :key
       :reader id :initarg :id)
   (url :type string
        :accessor url :initarg :url)
   (tags :type string
         :accessor tags :initarg :tags)))

(defpage links
  (:html
   (:body (dolist (link (all-links))
            (htm (:a :href (url link)
                     (str (url link)))
                 :br)))))

(defpage new-link
  (:html
   (:body
    (if (and (parameter "url"))
        (htm
         (add-link (parameter "url") (parameter "tags"))
         (:p "Link added"))
        (htm
         (:form :action "new-link" :method "post"
                (:p "URL:" (:input :type "text" :name "url"))
                (:p "tags:" (:input :type "text" :name "tags"))
                (:p (:input :type "submit" :value "add"))))))))

(defun all-links ()
  (select 'link :flatp t))

(defun get-max-id ()
  (reduce (lambda (last x)
            (max last x))
          (select [id] :from [link] :flatp t)
          :initial-value 0))

(let (id)
  (defun new-id ()
    (unless id
      (setf id (get-max-id)))
    (incf id)))

(defun add-link (url tags)
  (let ((link (make-instance 'link :id (new-id)
                             :url url :tags tags)))
    (clsql:update-records-from-instance link)))