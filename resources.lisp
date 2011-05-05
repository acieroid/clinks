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
                    ,@body
                    "Action performed with success")
                (clinks-error (e)
                  (setf (return-code*) (code e))
                  (format nil "~a" e))))))
        hunchentoot:*dispatch-table*))
