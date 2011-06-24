(in-package :clinks)

(defun hash (text &key (method :sha256) salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence method
                             (ironclad:ascii-string-to-byte-array
                              (if salt
                                  (concatenate 'string text salt)
                                  text)))))

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