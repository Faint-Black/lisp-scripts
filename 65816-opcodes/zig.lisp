;;-----------------------------------------------------------------------------
;;  Module which defines the [lisp opcode]->[zig opcode] format.
;;-----------------------------------------------------------------------------

(defun indent-level(N)
  (apply #'concatenate 'string
         (LOOP repeat N collect "    ")))

(defun bytecode-field(STR)
  (concatenate 'string "0x" STR))

(defun length-field(STR)
  (concatenate 'string "\"" STR "\""))

(defun cycles-field(STR)
  (concatenate 'string "\"" STR "\""))

(defun addressing-mode-field(STR)
  (cond
    ((string= STR "imp") ".implied")
    ((string= STR "imm") ".immediate")
    ((string= STR "dir") ".direct_page")
    ((string= STR "abs") ".absolute")
    ((string= STR "long") ".long")
    ((string= STR "rel8") ".relative_near")
    ((string= STR "rel16") ".relative_far")
    ((string= STR "src,dest") ".source_destination")
    (t (concatenate 'string "unknown enum: " STR))))

(defun flags-field(STR)
  (let*
      ((first-step (cl-ppcre:regex-replace-all "[mxi\\*]" STR "1"))
       (second-step (cl-ppcre:regex-replace-all "\\." first-step "0"))
       (third-step (concatenate 'string "0b" second-step)))
    third-step))

(defun emulation-field(STR)
  (if (string= STR ".") "0" "1"))

(defun syntax-field(STR)
  (concatenate 'string "\"" STR "\""))

(defun parse-to-zig-struct(OPCODE-LIST)
  (concatenate
   'string
   (indent-level 0) "pub const all_instructions = [256]Opcode{" (string #\Newline)
   (apply #'concatenate 'string
          (mapcar (lambda(OP)
                    (concatenate
                     'string
                     (indent-level 1) ".{" (string #\Newline)
                     (indent-level 2) ".bytecode = " (bytecode-field (opcode-code OP)) "," (string #\Newline)
                     (indent-level 2) ".length = " (length-field (opcode-length OP)) "," (string #\Newline)
                     (indent-level 2) ".cycles = " (cycles-field (opcode-cycles OP)) "," (string #\Newline)
                     (indent-level 2) ".addressing = " (addressing-mode-field (opcode-mode OP)) "," (string #\Newline)
                     (indent-level 2) ".flags = " (flags-field (opcode-flags OP)) "," (string #\Newline)
                     (indent-level 2) ".emulation = " (emulation-field (opcode-emulation OP)) "," (string #\Newline)
                     (indent-level 2) ".syntax = " (syntax-field (opcode-syntax OP)) "," (string #\Newline)
                     (indent-level 1) "}," (string #\Newline)))
                  OPCODE-LIST))
   (indent-level 0) "};" (string #\Newline)))
