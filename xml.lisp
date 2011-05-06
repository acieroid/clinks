(in-package :clinks)

(defparameter *tab-width* 2)

(defun split-attributes (args &optional attributes)
  (cond ((null args) (values attributes nil))
        ((keywordp (car args))
         (if (null (rest args))
             (error "Missing tag value: ~a" (car args))
             (split-attributes (cddr args)
                               (cons (cons (car args) (cadr args))
                                     attributes))))
        (t (values attributes args))))

(defun indent (stream n)
  (dotimes (i n)
    (write-char #\Space stream)))

(defun print-element (element stream indentation)
  (if (functionp element)
      (funcall element stream (+ indentation *tab-width*))
      (progn
        (indent stream (+ indentation *tab-width*))
        (princ element stream)))
  (terpri stream))

(defun <> (tag &rest args)
  (multiple-value-bind (attributes content) (split-attributes args)
    (lambda (stream n)
      (indent stream n)
      (format stream "<~(~a~)" tag)
      (dolist (attr attributes)
        (format stream " ~(~a~)=\"~a\"" (car attr) (cdr attr)))
      (if content
          (progn
            (write-char #\> stream)
            (terpri stream)
            (mapcar (lambda (el) (print-element el stream n)) content)
            (indent stream n)
            (format stream "</~(~a~)>~%" tag))
          (format stream "/>~%")))))

(defun xml (fun)
  (with-output-to-string (stream)
    (funcall fun stream 0)))

(defun <>iter (fun list)
  (lambda (stream n)
    (mapcar (lambda (el) (print-element (funcall fun el) stream n))
            list)))
