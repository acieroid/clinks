(in-package :clinks)

;; TODO: add a salt
(defun hash (text &optional (method :sha256))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence method
                             (ironclad:ascii-string-to-byte-array text))))

(defun now ()
  (local-time:timestamp-to-unix (local-time:now)))

