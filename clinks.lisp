(in-package #:clinks)

(defparameter *css-file* "/design.css")

;; For debug only
(setf *show-lisp-errors-p* t
      *hunchentoot-default-external-format* :utf-8
      *default-content-type* "text/html; charset=utf-8")

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
  (create-tables 'user 'link 'tag 'tag-join))

(defun start (&optional (port 8000) (db-specs '("clinks.db")) (db-type :sqlite3))
  (start-databases db-specs db-type)
  (push (create-static-file-dispatcher-and-handler "/design.css" *css-file*) *dispatch-table*)
  (hunchentoot:start
   (make-instance 'acceptor :port port)))
