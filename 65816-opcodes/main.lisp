;;-----------------------------------------------------------------------------
;;  This script automatically scrapes and organizes the 65816 opcodes into
;; digestible and sortable pieces of data, then outputs the ordered results
;; into a LaTeX formatted file.
;;-----------------------------------------------------------------------------

;; http request library
(ql:quickload :dexador)
;; regex matching library
(ql:quickload :cl-ppcre)
;; system utilities library
(ql:quickload :uiop)

;; Opcode struct
(defstruct opcode
  (code "ERROR" :type string)
  (length "ERROR" :type string)
  (cycles "ERROR" :type string)
  (mode "ERROR" :type string)
  (flags "ERROR" :type string)
  (emulation "ERROR" :type string)
  (syntax "ERROR" :type string))

(defun latex-verbatim(STR)
  (concatenate 'string "\\texttt{" STR "}"))
(defun latex-boldface(STR)
  (concatenate 'string "\\textbf{" STR "}"))

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

(defun process-syntax-string-list(STRINGLIST)
  "Inserts spaces between the syntax words"
  (cond
    ((<= (length STRINGLIST) 1) STRINGLIST)
    (t (append (list (car STRINGLIST) " ") (process-syntax-string-list (cdr STRINGLIST))))))

(defun get-opcode-strings(FILENAME)
  "Get opcodes as a sorted list of strings, matches with a regex"
  (let*
      ((str (uiop:read-file-string FILENAME))
       (matches-raw (cl-ppcre:all-matches-as-strings "(?m)^[A-F0-9][A-F0-9]\\s[0-9].*" str))
       (matches-sorted (sort matches-raw #'string<)))
    matches-sorted))

(defun parse-opcode-string(STRING)
  "Turns the opcode line into a opcode struct"
  (let*
      ((separated-str
         (cl-ppcre:split "\\s+" STRING))
       (corrected-str
         (concatenate 'list
                      (subseq separated-str 0 6)
                      (list (apply #'concatenate 'string
                                   (process-syntax-string-list (subseq separated-str 6))))))
       (op-struct
         (make-opcode
          :code (nth 0 corrected-str)
          :length (nth 1 corrected-str)
          :cycles (nth 2 corrected-str)
          :mode (nth 3 corrected-str)
          :flags (nth 4 corrected-str)
          :emulation (nth 5 corrected-str)
          :syntax (nth 6 corrected-str))))
    op-struct))

(defun parse-opcode-strings(STRINGS)
  "Evaluates all the opcodes into structs"
  (mapcar #'parse-opcode-string STRINGS))

(defun opcode-as-string(OP)
  "Format an opcode struct into a string"
  (format nil "  ~a & ~3a & ~11a & ~a & ~26a \\\\~%"
          (latex-verbatim (concatenate 'string "0x" (opcode-code OP)))
          (opcode-length OP)
          (opcode-cycles OP)
          (latex-verbatim
           (cl-ppcre:regex-replace-all
            "m"
            (cl-ppcre:regex-replace-all "x" (opcode-flags OP) "*")
            "*"))
          (latex-verbatim
           (cl-ppcre:regex-replace-all
            "#"
            (cl-ppcre:regex-replace-all "\\$" (opcode-syntax OP) "\\\$")
            "\\#"))))

(defun opcodes-as-latex-table(OPCODES)
  "Formats all the structs into a single LaTeX formatted table string"
  (concatenate 'string
               "\\begin{longtable}{|l|l|l|l|l|}" (string #\Newline)
               "  \\hline" (string #\Newline)
               "  \\rowcolor{lightgray}" (string #\Newline)
               "  Code & Length & Cycles & nvmxdizc & Syntax \\\\" (string #\Newline)
               "  \\hline" (string #\Newline)
               (apply #'concatenate 'string (mapcar #'opcode-as-string OPCODES))
               "  \\hline" (string #\Newline)
               "\\end{longtable}"))

;; Creates a text file with all opcodes in varying orders
(defun main()
  (download-resource "http://www.6502.org/tutorials/65c816opcodes.html" "65c816opcodes.html")
  (write-string-to-file "sorted_by_opcodes.txt"
                        (opcodes-as-latex-table
                         (parse-opcode-strings
                          (get-opcode-strings "65c816opcodes.html")))))
