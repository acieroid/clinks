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

(defun hunchentoot::default-handler ()
  (setf (content-type*) "text/xml; charset=utf-8")
  (setf (return-code*) 400)
  (log-message :info "Default handler called for script ~a" (script-name*))
  (format nil "<error>Invalid resource</error>"))

(defun start (&optional (port 8080) (db-specs '("clinks.db")) (db-type :sqlite3))
  (start-databases db-specs db-type)
  ;(create-folder-dispatcher-and-handler "/html/" "../html/")
  (mapcar (lambda (x)
            (push (create-static-file-dispatcher-and-handler
                   (first x) (second x))
                  *dispatch-table*))
          '(("/" "../html/index.html")
            ("/clinks.js" "../html/clinks.js")
            ("/interface.js" "../html/interface.js")
            ("/jquery.min.js" "../html/jquery.min.js")
            ("/jquery.cookie.js" "../html/jquery.cookie.js")
            ("/bookmarklet.js" "../html/bookmarklet.js")))
  (hunchentoot:start (make-instance 'acceptor :port port)))