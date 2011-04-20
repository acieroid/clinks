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

(defun << (tag &rest args)
  (flet ((stringify (x)
           (string-downcase (symbol-name x))))
    (multiple-value-bind (attributes content) (split-attributes args)
      (with-output-to-string (stream)
        (format stream "<~a" (stringify tag))
        (when attributes
          (princ " " stream))
        (mapcar (lambda (x)
                  (format stream "~a=\"~a\""
                          (stringify (car x))
                          (cdr x)))
                attributes)
        (format stream ">~%~{~a~%~}</~a>" content (stringify tag))))))

(defun <<iter (fun list)
  ;; TODO: might be inefficient with large input, maybe use a stream
  ;; and print directly to it
  (reduce (lambda (last el) (concatenate 'string last (funcall fun el))) list
          :initial-value ""))