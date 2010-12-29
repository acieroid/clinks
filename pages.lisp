(in-package :clinks)

(defparameter *menu-elements*
  '((links "All")
    (new-link "New link")))

(defun print-menu ()
  (with-html-output-to-string (stream)
    (:div :class "menu"
          (flet ((print-link (element)
                   (let ((link (first element))
                         (name (second element)))
                     (htm (:a :href (format nil "/~(~a~)" link) (str name))))))
            (mapcar (lambda (el) (print-link el) (htm (str " - ")))
                    *menu-elements*)
            (let ((user (session-value :user)))
              (if user
                  (htm "Connected as " (str user)
                       " " (print-link '(disconnect "(disconnect)")))
                  (htm "Disconnected: "
                       (print-link '(connect "Connect")) " - "
                       (print-link '(register "Register")))))))))

(defmacro def-blank-page (name &body body)
  (let ((stream (gensym "stream"))
        (fname (gensym (symbol-name name))))
    `(progn
       (push (create-prefix-dispatcher
              (format nil "/~(~a~)" ',name) ',fname)
             *dispatch-table*)
       (defun ,fname ()
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

(defmacro defpagel (name title &body body)
  `(defpage ,name ,title
     (if (current-user)
         (progn (htm,@body))
         (htm "You must be connected to see this page"))))

(defmacro defaction (name title arg &body body)
  `(defpage ,name ,title
     (let* ((,arg (subseq (request-uri*)
                          (1+ (position #\/ (request-uri*) :start 1)))))
       ,@body)))

(defun get-action-url (action arg)
  (concatenate 'string action "/" arg))

