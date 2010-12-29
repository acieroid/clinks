(in-package :clinks)

(def-view-class tag ()
  ((id :type integer :db-kind :key :initform nil
       :reader id)
   (link-id :type integer :initform 0 :initarg :link-id
            :accessor link-id)
   (name :type string
         :accessor tag-name :initarg :name)
   ;; Joins
   (link :db-kind :join :db-info (:join-class link :home-key link-id :foreign-key id :set nil)
         :accessor link)))

(defun url-for-tag (name)
  (concatenate 'string "tag/" name))

(defun split-tags (tags)
  (split-sequence:split-sequence #\Space tags))

(defmethod print-html ((tag tag))
  (with-html-output-to-string (stream)
    (:span :class "tag"
           (:a :href (url-for-tag (tag-name tag))
               (str (tag-name tag))))))

(defun create-tag (link-id name)
  (let ((tag (make-instance 'tag :link-id link-id :name name)))
    (update-records-from-instance tag)))
