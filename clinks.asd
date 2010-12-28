;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("hunchentoot" "clsql" "clsql-sqlite3" "cxml-rpc" "cl-who" "ironclad" "alexandria")
  :components ((:file "package")
               (:file "clinks")
               (:file "pages")
               (:file "users")
               (:file "links")
               (:file "api")))

