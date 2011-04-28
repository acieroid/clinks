(in-package :clinks)

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

(defun start-databases (db-specs db-type)
  (connect-db db-specs db-type)
  (create-tables 'user 'link))

(defun start (&optional (port 8080) (db-specs '("clinks.db")) (db-type :sqlite3))
  (let ((*handle-http-errors-p* nil)
        (*message-log-pathname* #p"/var/log/clinks.log")
        (*hunchentoot-default-external-format* :utf-8)
        (*default-content-type* "text/xml; charset=utf-8"))
    (start-databases db-specs db-type)
    (hunchentoot:start (make-instance 'acceptor :port port))))