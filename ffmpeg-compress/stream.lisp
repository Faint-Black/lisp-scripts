;;-----------------------------------------------------------------------------
;;  Module which defines shell stdout/stderr stream to string functions.
;;-----------------------------------------------------------------------------

(defun capture-stdout(COMMAND)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (output)
     (uiop:run-program
      COMMAND
      :output output
      :error-output *error-output*))))
