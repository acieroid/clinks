(in-package :clinks)

(setf *handle-http-errors-p* nil
      *message-log-pathname* #p"/tmp/clinks.log"
      *hunchentoot-default-external-format* :utf-8)

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
  (start-databases db-specs db-type)
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler "/" "../html/index.html")
   hunchentoot:*dispatch-table*)
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler "/clinks.js" "../html/clinks.js")
   hunchentoot:*dispatch-table*)
  (hunchentoot:start (make-instance 'acceptor :port port)))