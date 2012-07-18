(defpackage #:wallpaper-dl
  (:use #:cl
        #:drakma
        #:cl-ppcre
        #:html-parse)
  
  (:export #:download
           #:get-all-images-with-resolution))

(declaim (optimize (safety 3)) (optimize (debug 3)) (optimize (speed 0)))
(declaim (inline get-image-id get-href get-resolution prompt-read print-asape))