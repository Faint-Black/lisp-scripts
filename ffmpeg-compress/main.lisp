;;-----------------------------------------------------------------------------
;;  This script acts as a wrapper for a general purpose ffmpeg compression
;; script, without having to use their weird DSL.
;;-----------------------------------------------------------------------------

;; regex matching library
(ql:quickload :cl-ppcre :silent t)
;; system utilities library
(ql:quickload :uiop :silent t)

(defun capture-stdout(COMMAND)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (output)
     (uiop:run-program
      COMMAND
      :output output
      :error-output *error-output*))))

(defstruct video
  (name "UNDEFINED" :type string)
  (width -1 :type integer)
  (height -1 :type integer)
  (bitrate -1 :type integer)
  (framerate -1 :type integer))

(defun to-si-unit-string(NUM)
  (string-downcase (capture-stdout (format nil "numfmt --to=iec ~a" NUM))))

(defun from-si-unit-string(STRING)
  (values (parse-integer (capture-stdout (format nil "numfmt --from=iec ~a" STRING)))))

(defun parse-framerate(STRING)
  "'r_frame_rate=60/1' => '60'"
  (car (cl-ppcre:all-matches-as-strings "(?<==).*?(?=/)" STRING)))

(defun get-video-metadata(FILENAME)
  "Makes a video metadata struct out of a filepath"
  (make-video
   :name FILENAME
   :width (values (parse-integer (capture-stdout (concatenate 'string "ffprobe -v error -show_entries stream=width -of default=nw=1:nk=1 " FILENAME))))
   :height (values (parse-integer (capture-stdout (concatenate 'string "ffprobe -v error -show_entries stream=height -of default=nw=1:nk=1 " FILENAME))))
   :bitrate (values (parse-integer (capture-stdout (concatenate 'string "ffprobe -v error -show_entries format=bit_rate -of default=nw=1:nk=1 " FILENAME))))
   :framerate (values (parse-integer (parse-framerate (capture-stdout (concatenate 'string "ffprobe -v error -select_streams v:0 -show_entries stream=r_frame_rate -of default=noprint_wrappers=1 " FILENAME)))))))

(defun video-has-missing-field(VIDEO)
  "Check for errors in the video generation step"
  (cond
    ((equal (video-name VIDEO) "UNDEFINED") t)
    ((equal (video-width VIDEO) -1) t)
    ((equal (video-height VIDEO) -1) t)
    ((equal (video-bitrate VIDEO) -1) t)
    ((equal (video-framerate VIDEO) -1) t)
    (t nil)))

(defun resolution-downscale(WIDTH)
  (if (> WIDTH 720) (/ WIDTH 720) 1))

(defun get-suggested-compression-format(VIDEO)
  "Generates a new video struct with the suggested compression parameters"
  (let* ((downscale (resolution-downscale (video-width VIDEO)))
         (lowest-bitrate (if (< (video-bitrate VIDEO) (from-si-unit-string "128K")) (video-bitrate VIDEO) (from-si-unit-string "128K")))
         (lowest-fps (if (< (video-framerate VIDEO) 24) (video-framerate VIDEO) 24))
         (new-width (values (floor (/ (video-width VIDEO) downscale))))
         (new-height (values (floor (/ (video-height VIDEO) downscale)))))
    (make-video
     :name "compressed.mp4"
     :width (if (= (mod new-width 2) 1) (+ new-width 1) new-width)
     :height (if (= (mod new-height 2) 1) (+ new-height 1) new-height)
     :bitrate lowest-bitrate
     :framerate lowest-fps)))

(defun compression-script(OLDVIDEO NEWVIDEO)
  "Generates a ffmpeg script that compressed the oldvideo into the newvideo format"
  (format nil
          "ffmpeg -i ~a -vf \"scale=~a:~a\" -c:v libx264 -preset veryslow -r ~a -crf 27 -c:a aac -b:a ~a ~a"
          (video-name OLDVIDEO)
          (video-width NEWVIDEO)
          (video-height NEWVIDEO)
          (video-framerate NEWVIDEO)
          (to-si-unit-string (video-bitrate NEWVIDEO))
          (video-name NEWVIDEO)))

(defun video-as-string(VIDEO)
  (format nil "filepath: ~a, resolution: ~ax~a, fps: ~a, bitrate: ~a~a"
          (video-name VIDEO)
          (video-width VIDEO) (video-height VIDEO)
          (video-framerate VIDEO)
          (to-si-unit-string (video-bitrate VIDEO))
          (string #\Newline)))

(defun input-video-file-exists(FILENAME)
  (uiop:file-exists-p (concatenate 'string (namestring (uiop/os:getcwd)) FILENAME)))

(defun main(FILENAME)
  (if (not (input-video-file-exists FILENAME))
      (progn
        (format t "File does not exist!~%")
        (quit)))
  (let*
      ((input-video (get-video-metadata FILENAME))
       (output-video (get-suggested-compression-format input-video))
       (compression-cmd (compression-script input-video output-video))
       (bold (concatenate 'string (string #\Escape) "[1m"))
       (green (concatenate 'string (string #\Escape) "[1;32m"))
       (reset (concatenate 'string (string #\Escape) "[0m")))
    (format t "~aInput file:~a~%~a" bold reset
            (video-as-string input-video))
    (format t "~aOutput file:~a~%~a" bold reset
            (video-as-string output-video))
    (format t "~aRunning command:~a~%~a~%" bold reset
            compression-cmd)
    (uiop:run-program compression-cmd)
    (format t "~aAll done.~a~%" green reset)
    (format t "~aInput filesize~a = ~a~%" bold reset
            (capture-stdout (concatenate 'string "du -h " (video-name input-video))))
    (format t "~aOutput filesize~a = ~a~%" bold reset
            (capture-stdout (concatenate 'string "du -h " (video-name output-video))))))
