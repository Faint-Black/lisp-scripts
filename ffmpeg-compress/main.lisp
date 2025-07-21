;;-----------------------------------------------------------------------------
;;  This script acts as a wrapper for a general purpose ffmpeg compression
;; script, without having to use their weird DSL.
;;-----------------------------------------------------------------------------

;; system utilities library
(ql:quickload :uiop)

(load "./stream.lisp") ; shell command to string functions
(load "./video.lisp")  ; video and ffmpeg related functions

(defun main(FILENAME)
  (get-video-metadata FILENAME))
