(defun is-number (char)
  (and (>= (char-code char) (char-code #\0))
       (<= (char-code char) (char-code #\9))))

(defun next-char-is (stream char)
  (let ((next-char (read-char stream)))
    (when (not (eql next-char char))
      (error "parse error: got ~c instead of ~c" next-char char))))

(defun next-string-is (stream string)
  (loop for char across string
       do (next-char-is stream char)))

(defun read-integer (stream)
  (reduce (lambda (last x) (+ (* 10 last) x))
          (cons 0 (loop while (is-number (peek-char t stream))
                     collect (- (char-code (read-char stream))
                                (char-code #\0))))))
(defun read-string (stream)
  (let ((string (make-array 0
                            :element-type 'character
                            :fill-pointer 0
                            :adjustable t)))
    (next-char-is stream #\")
    (loop until (eql (peek-char t stream) #\")
       do (vector-push-extend (read-char stream) string))
    (read-char stream)
    string))

(defun starts-with (line str)
  (string= (subseq line 0 (length str)) str))

(defun skip-header (stream)
  (loop for line = (read-line stream)
       until (starts-with line "<H1>")))

(defun parse-link (string &optional (create-link-fun #'list))
  (with-input-from-string (stream string)
    (next-string-is stream "<DT>")
    (next-string-is stream "<A HREF=")
    (let ((link (read-string stream)))
      (next-string-is stream " ADD_DATE=")
      (let ((date (parse-integer (read-string stream))))
        (next-string-is stream " PRIVATE=")
        (let ((private (string= (read-string stream) "1")))
          (next-string-is stream " TAGS=")
          (let* ((tags (read-string stream))
                 (end (read-line stream))
                 (title (subseq end 1 (- (length end) 4))))
            (funcall create-link-fun link date private tags title)))))))

(defun parse-from-stream (stream create-link-fun)
  (skip-header stream)
  (next-string-is stream "<DL><p>")
  (loop for line = (read-line stream)
       until (starts-with line "</DL>")
       when (starts-with line "<DT>") ;; ignoring notes for the moment
       collect (parse-link line create-link-fun)))

(defun parse (file &optional (create-link-fun #'list))
  (with-open-file (stream file
                          :direction :input
                          :external-format :utf-8)
    (parse-from-stream stream create-link-fun)))