(in-package :clinks)

(defparameter *links-per-page* 10)
(defparameter *pages-links-shown* 10)

(def-view-class link ()
  ((id :type integer :db-kind :key :initform nil
       :reader id)
   (user-id :type integer :initarg :user-id)
   (url :type string :accessor url :initform "" :initarg :url)
   (title :type string :accessor title :initform "" :initarg :title)
   (notes :type string :accessor notes :initform "" :initarg :notes)
   (private :type boolean :accessor private :initform nil :initarg :private)
   (timestamp :type integer :accessor timestamp :initform 0 :initarg :timestamp)
   ;; Joins
   (user :db-kind :join :db-info (:join-class user :home-key user-id
                                  :foreign-key id :set nil)
         :accessor user)))

(defmethod date ((link link))
  (multiple-value-bind (s m h date month year day daylight tz)
      (decode-universal-time (timestamp link))
    (declare (ignore s m h day daylight tz))
    (format nil "~a-~a-~a" month date year)))

(defmethod print-html ((link link))
  (with-html-output-to-string (stream)
    (:div :class "link"
          (:a :href (url link)
              (str (title link)))
          " "
          (str (date link))
          " "
          (:a :href (get-action-url "edit" (url link)) "edit") " "
          (:a :href (get-action-url "delete" (url link))  "x")
          :br
          (:div :class "tags"
                (mapcar (lambda (tag) (htm (str (print-html tag)) " "))
                      (tags link)))
          (:div :class "notes"
                (str (notes link))))))

(defmethod username ((link link))
  (username (user link)))

(defun print-links (links)
  (with-html-output-to-string (stream)
    (:ul
     (mapcar (lambda (x)
               (htm (:li (str (print-html x)))))
             links))))

(defun range (x y)
  (loop for i from x to y collect i))

(defun print-pager (action n pages)
  (unless (= pages 0)
    (with-html-output-to-string (stream)
      (:div :class "pager"
            (let* ((first (max 0 (- n (/ *pages-links-shown* 2))))
                   (last (min (+ first *pages-links-shown*) pages)))
              (mapcar (lambda (x)
                        (if (= x n)
                            (str x)
                            (htm (:a :href  (get-action-url action
                                                            (format nil "~a" x))
                                     (str x))))
                        (str " "))
                      (range first last)))))))

(defun all-links ()
  (select 'link :flatp t :refresh t))

#.(locally-enable-sql-reader-syntax)
(defmethod tags ((link link))
  (mapcar #'first
          (select 'tag 'tag-join :where [and [= [slot-value 'tag-join 'link-id] (id link)]
                                             [= [slot-value 'tag-join 'tag-id] [slot-value 'tag 'id]]]
                  :refresh t :flatp t)))


(defun add-link (user
                 &key (url "") (title "") (tags "") (notes "") (private nil)
                      (timestamp (get-universal-time)))
  (let ((link (make-instance 'link :url url
                             :title title :notes notes
                             :private private
                             :user-id (id user)
                             :timestamp timestamp)))
    (update-records-from-instance link)
    ;; The id is set in the db, not in our object
    (let ((id (first (select [max [id]] :from 'link :flatp t :refresh t)))
          (tags-list (split-tags tags)))
      (mapcar (curry #'create-tag id) tags-list))))

(defun edit-link (link url title tags notes private)
  (setf (url link) url
        (title link) title
        (notes link) notes
        (private link) private)
  (update-records-from-instance link)
  ;; TODO: let tags without any joins to them ?
  (delete-records :from [tag-join] :where [= [link-id] (id link)])
  (mapcar (curry #'create-tag (id link)) tags))

(defun find-link (url user)
  (first (select 'link :where [and [= [user-id] (id user)]
                                   [= [url] url]]
                 :refresh t :flatp t)))

(defun user-links (user)
  (select 'link :where [= [user-id] (id user)]
          :refresh t :flatp t))

(defun n-pages (links)
  (floor (length links) *links-per-page*))

(defun links-at-page (links page)
  (let ((first-at (* page *links-per-page*))
        (last-at (min (* (1+ page) *links-per-page*)
                      (length links))))
    (subseq links first-at last-at)))

#.(disable-sql-reader-syntax)

(defun delete-link (url user)
  (let ((link (find-link url user)))
    (unless link
      (error "No such link ~a" url))
    (let ((tags (tags link)))
      (delete-instance-records link)
      (mapcar #'delete-instance-records tags))))

(defun get-page-title (url)
  (multiple-value-bind (string results)
      (ppcre:scan-to-strings "<title>\\s*([^\\s]*)\\s*</title>"
                             (drakma:http-request url))
    (declare (ignore string))
    (if results
        (aref results 0)
        "")))

(defun link-form (page name &optional (link (make-instance 'link)))
  (with-html-output-to-string (stream)
    (:form :action page :method "post"
           (:p "URL:" (:input :type "text" :name "url" :value (url link)))
           (:p "Title: " (:input :type "text" :name "title" :value (title link))
               " (will be filled automatically)")
           (:p "Tags:" (:input :type "text" :name "tags"
                               :value (format nil "~{~a ~}"
                                              (mapcar #'tag-name (tags link))))
               " (space delimited)")
           (:p "Notes: " (:textarea :name "notes" :value (notes link)))
           (:p "Private: "
               (if (private link)
                   ;; Whatever is the checked value, it's always checked :(
                   (htm (:input :type "checkbox" :name "private" :checked ""))
                   (htm (:input :type "checkbox" :name "private)"))))
           (:p (:input :type "submit" :value name)))))

(defpagel new-link "New link"
  (if (and (parameter "url") (not (string= (parameter "url") "")))
      (htm
       (add-link (current-user) :url (parameter "url")
                 :title (if (string= (parameter "title") "")
                            (get-page-title (parameter "url"))
                            (parameter "title"))
                 :tags (parameter "tags")
                 :notes (parameter "notes")
                 :private (string= (parameter "private") "on"))
       (:p "Link added"))
      (str (link-form "new-link" "Add"))))

(defaction edit "Edit" (url)
  (let ((link (find-link url (current-user))))
    (if (and (parameter "url") (not (string= (parameter "url") "")))
        (htm
         (edit-link link (parameter "url") (parameter "title")
                    (split-tags (parameter "tags"))
                    (parameter "notes")
                    (string= (parameter "private") "on"))
         "Link edited")
        (str (link-form (request-uri*) "Edit" link)))))

(defaction delete "Delete" (url)
  (delete-link url (current-user))
  "Link deleted")

(defaction links "My links" (&optional (page "0"))
  (let ((links (user-links (current-user)))
        (page (parse-integer page)))
    (str (print-links (links-at-page (user-links (current-user)) page)))
    (str (print-pager "links" page (n-pages links)))))


(defun filter-links (&key user tags url)
  (let ((links (all-links)))
    (when user
      (delete-if-not (curry #'string= (username user)) links
                     :key (compose #'username #'user)))
    (when url
      (delete-if-not (curry #'string= url) links))
    ;; This is ugly as shit
    (mapcar (lambda (tag)
              (delete-if-not (lambda (link)
                               (find tag (tags link)
                                     :test (lambda (name tag)
                                             (string= name (tag-name tag)))))
                             links))
            tags)
    links))

(defaction tag "Tag" (&rest tags)
  (let* ((links (filter-links :user (current-user) :tags tags)))
    (htm (:ul (mapcar (lambda (x)
                        (htm (:li (str (print-html x)))))
                      links)))))

