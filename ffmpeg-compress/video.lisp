;;-----------------------------------------------------------------------------
;;  Module which defines the video metadata contents and ffmpeg scripts.
;;-----------------------------------------------------------------------------

(defstruct video
  (name "UNDEFINED" :type string)
  (width "UNDEFINED" :type string)
  (height "UNDEFINED" :type string)
  (bitrate "UNDEFINED" :type string))

(defun get-video-metadata(FILENAME)
  (make-video
   :name FILENAME
   :width (capture-stdout (concatenate 'string "ffprobe -v error -show_entries stream=width -of default=nw=1:nk=1 " FILENAME))
   :height (capture-stdout (concatenate 'string "ffprobe -v error -show_entries stream=height -of default=nw=1:nk=1 " FILENAME))
   :bitrate (capture-stdout (concatenate 'string "ffprobe -v error -show_entries format=bit_rate -of default=nw=1:nk=1 " FILENAME))))
