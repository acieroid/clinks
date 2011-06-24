(in-package :clinks)

(defparameter *tag-separator* #\,)

(defclass tag ()
  ((name :accessor name :initarg :name)
   (user :accessor user :initarg :user)
   (links :initarg :links)))

(defmethod links ((tag tag))
  (if (slot-boundp tag 'links)
      (slot-value tag 'links)
      (setf (slot-value tag 'links)
            (find-links (user tag) (list (name tag))))))

(defmethod get-href ((tag tag))
  (format nil "/users/~a/tags/~a" (username (user tag)) (name tag)))

(defun split-tags (tags)
  (split-sequence:split-sequence *tag-separator* tags))

(defun tags-from-string (user string)
  (mapcar (lambda (tag)
            (make-instance 'tag :name tag :user user))
          (split-tags string)))

;;; Database manipulation
#.(locally-enable-sql-reader-syntax)
(defun find-links (user tags)
  (flet ((cat (&rest str)
           (apply #'concatenate 'string str)))
    (let ((props (apply (sql-operator 'and)
                        (mapcar (lambda (tag)
                                  [or [like [tags] tag]
                                  [like [tags] (cat tag ",%")]
                                  [like [tags] (cat "%," tag)]
                                  [like [tags] (cat "%," tag ",%") ]])
                                tags))))
      (select 'link :where [and [= [user-id] (id user)] props ]
              :refresh t :flatp t))))

#.(restore-sql-reader-syntax-state)

;;; Representations
(defmethod print-representation ((type (eql 'tags)) tags)
  (xml (<> 'tags
           (<>iter (lambda (tag)
                     (<> 'tag :href (get-href tag)
                         (name tag)))
                   tags))))

(defmethod print-representation ((type (eql 'tag)) tag)
  (xml (<> 'tag
           (<> 'name (name tag))
           (print-representation 'links (links tag)))))

(defmethod parse-representation ((type (eql 'tag)) string)
  (parse-fields `((name "^(<tag>)$" ,#'identity))
                'tag
                string))

;;; Resources
(defresource :GET "^/users/(<username>)/tags/(<tags>)/?$" (username tags)
  (let ((user (find-user username)))
    (unless user
      (error 'user-doesnt-exists :username username))
    (print-representation 'links
                          (links (tags-from-string user tags)))))