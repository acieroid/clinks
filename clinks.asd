;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("alexandria" "split-sequence" "local-time"
               "hunchentoot" "clsql" "clsql-sqlite3"
               "ironclad" "s-xml")
  :components ((:file "package")
               (:file "xml")
               (:file "utils")
               (:file "resources")
               (:file "data")
               (:file "user")
               (:file "link")))

