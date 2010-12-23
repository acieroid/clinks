(in-package :clinks)

(defvar *pages* nil)
(defvar *menu-elements*
  '((links "all")
    (new-link "new")))

(defun print-menu ()
  (with-html-output-to-string (stream)
    (:div :class "menu"
          (flet ((print-link (element)
                   (let ((link (first element))
                         (name (second element)))
                     (htm (:a :href (format nil "/~(~a~)" link) (str name))))))
            (mapcar (lambda (el) (print-link el) (htm (str " - ")))
                    (butlast *menu-elements*))
            (print-link (car (last *menu-elements*)))))))

(defmacro def-blank-page (name &body body)
  (let ((stream (gensym "stream")))
    `(progn
       (setf *pages* (union *pages* (list ',name)))
       (push (create-prefix-dispatcher
              (format nil "/~(~a~)" ',name) ',name)
             *dispatch-table*)
       (defun ,name ()
         (with-html-output-to-string (,stream nil :indent t)
           ,@body)))))

(defmacro defpage (name title &body body)
  `(def-blank-page ,name
     (:html
      (:head
       (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
       (:link :type "text/css" :rel "stylesheet" :href "design.css")
       (:title ,title))
      (:body
       (:h1 ,title)
       (str (print-menu))
       ,@body))))

