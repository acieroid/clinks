(in-package :clinks)

(defgeneric print-representation (type object)
  (:documentation "Print the XML representation of an object"))

(defgeneric rfc3339 (object)
  (:documentation "Return a string containing the creation date of the object")
  (:method (object)
    (local-time:format-rfc3339-timestring
     nil
     (local-time:unix-to-timestamp (timestamp object)))))

(defgeneric id (object)
  (:documentation "Return the id of OBJECT"))

(defgeneric get-href (object)
  (:documentation "Return the link to OBJECT's representation")
  (:method (object)
    (format nil "~a/~a" (class-name (class-of object)) (id object))))