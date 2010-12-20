;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("hunchentoot" "clsql" "clsql-sqlite3" "cxml-rpc" "cl-who")
  :components ((:file "package")
               (:file "clinks")
               (:file "links")))

