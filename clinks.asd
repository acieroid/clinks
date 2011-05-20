;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("alexandria" "split-sequence" "local-time"
               "hunchentoot" "clsql" "clsql-sqlite3" "closer-mop"
               "ironclad" "s-xml")
  :components ((:file "package")
               (:file "xml")
               (:file "utils")
               (:file "errors")
               (:file "regex")
               (:file "resources")
               (:file "data")
               (:file "user")
               (:file "link")
               (:file "tag")
               (:file "clinks")))

