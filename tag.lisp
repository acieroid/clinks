(in-package :clinks)

(defclass tag ()
  ((name :accessor name :initarg :name)
   (links :accessor links :initarg :links)))

;;; Database manipulation
#.(locally-enable-sql-reader-syntax)
(defun find-tag (user tag)
  (flet ((cat (&rest str)
           (apply #'concatenate 'string str)))
    (make-instance 'tag :name tag
                   :links
                   (select 'link :where [and [= [user-id] (id user)]
                                             [or [like [tags] tag]
                                                 [like [tags] (cat tag ",%")]
                                                 [like [tags] (cat "%," tag)]
                                                 [like [tags] (cat "%," tag ",%")]]]
                           :refresh t :flatp t))))

#.(restore-sql-reader-syntax-state)

;;; Representations
(defmethod print-representation ((type (eql 'tag)) tag)
  (xml (<> 'tag
           (<> 'name (name tag))
           (print-representation 'links (links tag)))))

(defmethod parse-representation ((type (eql 'tag)) string)
  (parse-fields `((name "^(<tag>)$" ,#'identity))
                'tag
                string))

;;; Resources
(defresource :GET "^/users/(<username>)/tags/(<tag>)/?$" (username name)
  (let ((user (find-user username)))
    (unless user
      (error 'user-doesnt-exists :username username))
    (print-representation 'tag (find-tag user name))))