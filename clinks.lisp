(in-package #:clinks)

(defun find-db (name)
  (find-if (lambda (db)
             (string= (database-name db) name))
           (connected-databases)))

(defun connect-db (specs type)
  (unless (find-db (database-name-from-spec specs type))
    (connect specs :database-type type)))

(defun create-tables (&rest tables)
  (mapcar (lambda (table)
            (unless (table-exists-p table)
              (create-view-from-class table)))
          tables))
(defun create-dispatchers (names)
  (setf hunchentoot:*dispatch-table*
        (mapcar (lambda (name)
                  (hunchentoot:create-prefix-dispatcher
                   ;; TODO not capitals
                   (format nil "/~a" name) name))
                names)))

(defun start (&optional (port 8000) (db-specs '("clinks.db")) (db-type :sqlite3))
  (connect-db db-specs db-type)
  (create-tables 'link)
  (create-dispatchers '(links))
  (hunchentoot:start
   (make-instance 'hunchentoot:acceptor :port port)))
