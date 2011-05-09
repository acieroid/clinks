(in-package :clinks)

(defun create-rest-dispatcher (method regex handler)
  (let ((scanner (ppcre:create-scanner regex)))
    (lambda (request)
      (and (ppcre:scan scanner (script-name request))
           (eq method (request-method*))
           handler))))

(defmacro defresource (method regex variables &body body)
  `(push (create-rest-dispatcher
          ,method ,regex
          (lambda ()
            (destructuring-bind ,variables
               (coerce
                (second (multiple-value-list
                         (ppcre:scan-to-strings ,regex (script-name*))))
                'list)
              (setf (hunchentoot:content-type*) "text/xml")
              (handler-case
                  (progn
                    (let ((result (progn ,@body)))
                      (if (stringp result)
                          result
                          (format nil "Action performed with success~%"))))
                (clinks-error (e)
                  (setf (return-code*) (code e))
                  (format nil "~a~%" e))))))
        hunchentoot:*dispatch-table*))

(defmacro defresource-logged (user method regex variables &body body)
  (let ((auth-username (gensym "auth-username")) ; authentification username
        (req-username (gensym "req-username")) ; requested username
        (password (gensym "password")))
    `(defresource ,method
         ,(concatenate 'string "^/users/([a-zA-Z0-9]+)" regex)
         ,(cons req-username variables)
       (multiple-value-bind (,auth-username ,password) (authorization)
         (unless ,auth-username
           (error 'not-logged))
         (if (good-password-p ,auth-username ,password)
             (let ((,user (find-user ,auth-username)))
               (unless (string= ,auth-username ,req-username)
                 (error 'not-your-user :username ,req-username))
               ,@body)
             (error 'wrong-password :username ,req-username))))))