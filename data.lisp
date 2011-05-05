(in-package :clinks)

;;; Methods
(defgeneric print-representation (type object)
  (:documentation "Print the XML representation of an object"))

(defgeneric parse-representation (type xml)
  (:documentation "Parse a XML representation of an object and return the object created"))

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

(defun parse-fields (spec class string)
  (let ((instance (make-instance class))
        (current-field nil))
    (flet ((new-element (name attributes seed)
             (declare (ignore attributes))
             (setf current-field (intern (string-upcase (symbol-name name))))
             seed)
           (text (string seed)
             (let ((element (assoc current-field spec)))
               (if element
                   (if (ppcre:scan (second element) string)
                       (setf (slot-value instance current-field)
                             (funcall (third element) string))
                       (error 'forbidden-characters :field current-field))
                   (error 'unknown-field :field current-field)))
             seed))
      (with-input-from-string (stream string)
        ;;; We have to bind *package* to avoid having symbol interned
        ;;; in the wrong package by s-xml
        (let ((*package* (find-package :clinks)))
          (s-xml:start-parse-xml stream
                                 (make-instance 's-xml:xml-parser-state
                                                :new-element-hook #'new-element
                                                :text-hook #'text)))))
    instance))

;;; Patch clsql (TODO: fill that bug in the mailing list)
(in-package :clsql-sys)
(defmethod database-get-type-specifier ((type (eql 'integer))
                                        args database
                                        (db-type (eql :sqlite3)))
  (declare (ignore database))
  (if args
      (format nil "INTEGER(~A)" (car args))
      "INTEGER"))
(in-package :clinks)

;;; Parent class of all data's classes
(def-view-class data ()
  ((id :type integer :db-kind :key :db-constraints :not-null :reader id :initform nil)
   (timestamp :type integer :accessor timestamp :initform 0)))

(defmethod initialize-instance :after ((data data) &key &allow-other-keys)
  (setf (timestamp data) (now)))
