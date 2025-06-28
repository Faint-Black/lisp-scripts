;;-----------------------------------------------------------------------------
;;  Module which defines the Lisp opcode struct and its parsing options.
;;-----------------------------------------------------------------------------

(defstruct opcode
  (code "ERROR" :type string)
  (length "ERROR" :type string)
  (cycles "ERROR" :type string)
  (mode "ERROR" :type string)
  (flags "ERROR" :type string)
  (emulation "ERROR" :type string)
  (syntax "ERROR" :type string))

(defun process-syntax-string-list(STRINGLIST)
  "Inserts spaces between the syntax words"
  (cond
    ((<= (length STRINGLIST) 1) STRINGLIST)
    (t (append (list (car STRINGLIST) " ") (process-syntax-string-list (cdr STRINGLIST))))))

(defun get-opcode-strings(FILENAME)
  "Get opcodes as a sorted list of strings, matched with a regex"
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
