(in-package :clinks)

(defun create-rest-dispatcher (method regex handler)
  (let ((scanner (ppcre:create-scanner regex)))
    (lambda (request)
      (and (eq method (request-method*))
           (ppcre:scan scanner (script-name request))
           handler))))

(defmacro defresource (method regex variables &body body)
  `(push (create-rest-dispatcher
          ,method ,(replace-regexes regex)
          (lambda ()
            (destructuring-bind ,variables
               (coerce
                (second (multiple-value-list
                         (ppcre:scan-to-strings ,(replace-regexes  regex)
                                                (script-name*))))
                'list)
              (handler-case
                  (progn
                    (setf (hunchentoot:content-type*) "text/xml; charset=utf-8")
                    (log-message 'info "Resource accessed: ~a" (script-name*))
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
         ,(concatenate 'string "^/users/(<username>)" regex)
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