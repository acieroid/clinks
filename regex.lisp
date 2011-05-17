(in-package :clinks)

(defparameter *regexes*
  '((word . "\\w+")
    (everything . "[^<>]+")
    (username . "<word>")
    (password . "<word>")
    (url . "[a-zA-Z0-9\-\.\:]+")
    (title . "<everything>")
    (notes . "<everything>")
    (tag . "<word>")
    (tags . "(<tag>,?)+")))

(defun get-regex (string)
  (let ((symbol (intern
                 (string-upcase
                  (subseq string 1 (1- (length string)))))))
    (cdr (assoc symbol *regexes*))))


(defun replace-matches (string matches)
  (let ((result ""))
    (do ((again t matches)
         (last 0 end)
         (start (pop matches) (pop matches))
         (end (pop matches) (pop matches)))
        ((when (not again)
           (setf result (concatenate 'string
                                     result
                                     (subseq string last)))))
      (setf result (concatenate 'string
                                result
                                (subseq string last start)
                                (get-regex
                                 (subseq string start end)))))
    result))

(defun replace-regexes (string)
  (let ((matches (ppcre:all-matches "<[a-z]+>" string)))
    (if matches
        (replace-regexes (replace-matches string matches))
        string)))