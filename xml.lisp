(in-package :clinks)

(defun split-attributes (args &optional attributes)
  (cond ((null args) (values attributes nil))
        ((keywordp (car args))
         (if (null (rest args))
             (error "Missing tag value: ~a" (car args))
             (split-attributes (cddr args)
                               (cons (cons (car args) (cadr args))
                                     attributes))))
        (t (values attributes args))))

(defun xml-elem (tag &rest args)
  (multiple-value-bind (attributes content) (split-attributes args)
    (with-output-to-string (stream)
      (format stream "<~a" tag)
      (when attributes
        (princ " " stream))
      (mapcar (lambda (x)
                (format stream "~a=\"~a\""
                        (string-downcase (symbol-name (car x)))
                        (cdr x)))
              attributes)
      (format stream ">~%~{~a~%~}</~a>" content tag))))

(defmacro << (tag &rest args)
  `(xml-elem ,(string-downcase (symbol-name tag)) ,@args))