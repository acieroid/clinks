(in-package #:clinks)

(defparameter *css-file* "design.css")

;; For debug only
(setf *show-lisp-errors-p* t)

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

(defun start (&optional (port 8000) (db-specs '("clinks.db")) (db-type :sqlite3))
  (connect-db db-specs db-type)
  (create-tables 'user 'link)
  (push (create-static-file-dispatcher-and-handler "/design.css" *css-file*) *dispatch-table*)
  (hunchentoot:start
   (make-instance 'acceptor :port port)))
