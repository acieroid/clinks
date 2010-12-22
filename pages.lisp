(in-package :clinks)

(defvar *pages* nil)

(defmacro def-blank-page (name &body body)
  (let ((stream (gensym "stream")))
    `(progn
       (setf *pages* (union *pages* (list ',name)))
       (push (create-prefix-dispatcher
              (format nil "/~(~a~)" ',name) ',name)
             *dispatch-table*)
       (defun ,name ()
         (with-output-to-string (,stream)
           (with-html-output (,stream nil :indent t)
             ,@body))))))

(defmacro defpage (name title &body body)
  `(def-blank-page ,name
     (:html
      (:head
       (:meta :http-equiv "Conte-Type" :content "text/html;charset=utf-8")
       (:link :type "text/css" :rel "stylesheet" :href "design.css")
       (:title ,title))
      (:body ,@body))))

