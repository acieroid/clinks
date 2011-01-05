(in-package :clinks)

(defparameter *tags-separator* #\Space)

(def-view-class tag ()
  ((id :type integer :db-kind :key :initform nil
       :reader id)
   (name :type string :accessor tag-name :initarg :name)))

(def-view-class tag-join ()
  ((link-id :type integer :initform 0 :initarg :link-id
            :accessor link-id)
   (tag-id :type integer :initform 0 :initarg :tag-id
           :accessor tag-id)))

(defun url-for-tag (name)
  (concatenate 'string "tag/" name))

(defun split-tags (tags &key (separator *tags-separator*))
  (split-sequence:split-sequence separator tags :remove-empty-subseqs t))

(defmethod print-html ((tag tag))
  (with-html-output-to-string (stream nil :indent t)
    (:span :class "tag"
           (:a :href (url-for-tag (tag-name tag))
               (str (tag-name tag))))))

#.(locally-enable-sql-reader-syntax)
(defun find-tag (name)
  (first (select 'tag :where [= [name] name]
                 :flatp t :refresh t)))
#.(disable-sql-reader-syntax)

(defun create-tag (link-id name)
  (unless (find-tag name)
    (let ((tag (make-instance 'tag :name name)))
      (update-records-from-instance tag)))
  (let* ((tag-id (id (find-tag name)))
         (join (make-instance 'tag-join :link-id link-id :tag-id tag-id)))
    (update-records-from-instance join)))
