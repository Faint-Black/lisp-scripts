;;-----------------------------------------------------------------------------
;;  Module which defines the HTTP and file utilities.
;;-----------------------------------------------------------------------------

(defun write-string-to-file(FILENAME STR)
  "Puts a string into a file"
  (with-open-file (stream FILENAME
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string STR stream)))

(defun get-resource(URL)
  "Returns an HTTP request in string form"
  (multiple-value-bind (http-body http-status) (dex:get URL)
    (if (= http-status 200) ; 200 is the success status code btw
        http-body
        (error "HTTP error: ~a" http-status))))

(defun download-resource(URL FILENAME)
  "If the file does not already exists, get and download it"
  (if (not (probe-file FILENAME)) (write-string-to-file FILENAME (get-resource URL))))
