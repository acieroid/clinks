(in-package :clinks)

(defpagel import "Import"
  (if (parameter "file")
      (let ((*create-link-fun* (curry #'add-link (current-user)))
            (*tags-separator* #\,))
        (parse (parameter "file")))
      (htm
       (:form :action "import" :method "post"
              (:p "File:" (:input :type "file" :name "file"))
              (:p (:input :type "submit" :value "Import"))))))