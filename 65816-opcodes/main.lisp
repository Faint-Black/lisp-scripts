;;-----------------------------------------------------------------------------
;;  This script automatically scrapes and organizes the 65816 opcodes into
;; digestible and sortable pieces of data, then outputs the resulting string
;; into a Zig struct format file, for use in a personal project of mine.
;;-----------------------------------------------------------------------------

;; http request library
(ql:quickload :dexador)
;; regex matching library
(ql:quickload :cl-ppcre)
;; system utilities library
(ql:quickload :uiop)

(load "./http.lisp")
(load "./opcode.lisp")
(load "./zig.lisp")

;; Creates a text file with all opcodes in varying orders
(defun main()
  (download-resource "http://www.6502.org/tutorials/65c816opcodes.html" "65c816opcodes.html")
  (write-string-to-file "instructions.txt"
                        (parse-to-zig-struct
                         (parse-opcode-strings
                          (get-opcode-strings "65c816opcodes.html")))))
