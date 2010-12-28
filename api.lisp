(in-package :clinks)

(push :result *html-empty-tags*)

(defmacro defapi (name args &body body)
  "Define a new API function. Bind the symbol USER."
  (flet ((convert-arg (symbol)
           (list symbol
                 `(parameter ,(format nil "~(~a~)" symbol)))))
    (let ((stream (gensym "stream"))
          (fname (gensym (symbol-name name)))
          (username (gensym "username"))
          (password (gensym "password")))
      `(progn
         (push (create-prefix-dispatcher
                (format nil "/~(~a~)" ',name) ',fname)
               *dispatch-table*)
         (defun ,fname ()
           (setf (hunchentoot:content-type*) "text/plain")
           (with-html-output-to-string (,stream)
             (multiple-value-bind (,username ,password) (authorization)
               (if (right-user-ids-p ,username (hash-password,password))
                   (handler-case (progn
                                   (let ((user (find-user ,username))
                                         ,@(mapcar #'convert-arg args))
                                     ,@body))
                     ;; TODO: use specific conditions?
                     (error (e) (htm (:result :code "something went wrong"
                                              :error (str e)))))
                   (htm (:result :code "access denied"))))))))))

(defapi posts/add (url tags)
  (add-link url (split-tags tags) user)
  (htm (:response :code "done")))

(defapi posts/delete (url)
  (delete-link url user)
  (htm (:response :code "done")))
