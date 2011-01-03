;;;; clinks.asd

(asdf:defsystem #:clinks
  :serial t
  :depends-on ("alexandria" "split-sequence"
               "hunchentoot" "cl-who"
               "clsql" "clsql-sqlite3"
               "ironclad" "drakma" "cl-ppcre")
  :components ((:file "package")
               (:file "clinks")
               (:file "pages")
               (:file "users")
               (:file "tags")
               (:file "links")
               (:file "api")
               (:file "parser")
               (:file "import")))

