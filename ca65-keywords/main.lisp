;;-----------------------------------------------------------------------------
;;  This script automatically scrapes the language keywords of the ca65
;; macro assembler from their official website.
;;-----------------------------------------------------------------------------

;; http request library
(ql:quickload :dexador)
;; regex matching library
(ql:quickload :cl-ppcre)
;; system utilities library
(ql:quickload :uiop)

;; Returns HTTP request in string form
(defun get-resource(URL)
  (multiple-value-bind (http-body http-status) (dex:get URL)
    (if (= http-status 200) ; 200 is the success status code btw
        http-body
        (error "HTTP error: ~a" http-status))))

;; If file does not exist, get and download it
(defun download-resource(URL FILENAME)
  (if (not (probe-file FILENAME))
      (with-open-file (stream FILENAME
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string (get-resource URL) stream))))

;; Get keywords as a list of strings
(defun get-keywords(FILENAME)
  (let*
      ((str (uiop:read-file-string FILENAME))
       (matches-raw (cl-ppcre:all-matches-as-strings "\\.[A-Z][A-Z0-9]+" str))
       (matches-no-dup (remove-duplicates matches-raw :test #'string=))
       (matches-sorted (sort matches-no-dup #'string<)))
    matches-sorted))

;; Returns the full list of language keywords of the ca65 assembler
(defun main()
  (download-resource "https://cc65.github.io/doc/ca65.html" "ca65.html")
  (get-keywords "ca65.html"))
