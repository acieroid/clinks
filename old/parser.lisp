(in-package :clinks)
(defparameter *create-link-fun* #'list)

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

(defun unix-to-universal-time (time)
  (+ time 2208988800))

(defun parse-link (string notes)
  (with-input-from-string (stream string)
    (next-string-is stream "<DT>")
    (next-string-is stream "<A HREF=")
    (let ((url (read-string stream)))
      (next-string-is stream " ADD_DATE=")
      (let ((timestamp (unix-to-universal-time
                        (parse-integer (read-string stream)))))
        (next-string-is stream " PRIVATE=")
        (let ((private (string= (read-string stream) "1")))
          (next-string-is stream " TAGS=")
          (let* ((tags (read-string stream))
                 (end (read-line stream))
                 (title (subseq end 1 (- (length end) 4)))
                 (notes (subseq notes 4)))
            (funcall *create-link-fun*
                     :url url :timestamp timestamp
                     :private private :tags tags
                     :title title :notes notes)))))))

(defun parse-from-stream (stream)
  (let (links)
    (skip-header stream)
    (next-string-is stream "<DL><p>")
    (do ((line (read-line stream) next-line)
         (next-line (read-line stream) (read-line stream)))
        ((or (starts-with line "</DL>") (starts-with next-line "</DL>")))
      (when (starts-with line "<DT>")
        (push (parse-link line
                          (if (starts-with next-line "<DD>")
                              next-line
                              "<DD>"))
              links)))
    (nreverse links)))

(defun parse (file)
  (with-open-file (stream file
                          :direction :input
                          :external-format :utf-8)
    (parse-from-stream stream)))