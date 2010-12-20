(in-package :clinks)

(defvar *pages* nil)

(defmacro defpage (name &body body)
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
