(in-package :clinks)

;; TODO: add a salt
(defun hash (text &optional (method :sha256))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence method
                             (ironclad:ascii-string-to-byte-array text))))

(defun now ()
  (local-time:timestamp-to-unix (local-time:now)))

(defgeneric merge-instances (new old)
  (:documentation "Merge two instances of a class")
  (:method (new old)
    (assert (eql (class-of new) (class-of old)))
    (log-message 'debug "Merging ~a and ~a" new old)
    (mapcar
     (lambda (slot)
       (let ((slot-name (closer-mop:slot-definition-name slot)))
         (when (slot-boundp new slot-name)
           (setf (slot-value old slot-name) (slot-value new slot-name)))))
     (closer-mop:class-direct-slots (class-of new)))
    old))