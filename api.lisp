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

(defapi posts/add (url description extended tags shared)
  ;; differences with delicious: replace and dt
  (add-link user :url url :title description :tags tags :notes extended
            :private (string= shared "yes"))
  (htm (:response :code "done")))

(defapi posts/delete (url)
  (delete-link url user)
  (htm (:response :code "done")))

(defapi posts/get (tag url)
  (flet ((space-separated-tags (tags)
           (reduce (lambda (last tag)
                     (concatenate 'string last " " (tag-name tag)))
                   tags)))
    (htm
     (:posts :user (username user)
             (mapcar (lambda (l)
                       (htm (:post
                             :href (url l)
                             :description (title l)
                             :extended (notes l)
                             :tag (space-separated-tags (tags l)))))
                     (filter-links :user user :tags (split-tags tags)
                                   :url (unless (string= url "") url)))))))