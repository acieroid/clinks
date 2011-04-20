;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("alexandria" "split-sequence" "local-time"
               "hunchentoot" "clsql" "clsql-sqlite3"
               "ironclad")
  :components ((:file "package")
               (:file "xml")
               (:file "utils")
               (:file "data")
               (:file "user")
               (:file "link")))

