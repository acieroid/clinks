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

(defun all-links ()
  ;; select return a list of "list of length 1", so we transform it
  ;; into a simple list
  (mapcar #'car (select 'link)))

(defun get-max-id ()
  (reduce (lambda (last x)
            (max last (first x)))
          (select [id] :from [link])
          :initial-value 0))

(let (id)
  (defun new-id ()
    (unless id
      (setf id (get-max-id)))
    (incf id)))

(defun new-link (url tags)
  (let ((link (make-instance 'link :id (new-id)
                             :url url :tags tags)))
    (update-records-from-instance link)))