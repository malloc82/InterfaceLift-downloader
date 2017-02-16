(defpackage #:wallpaper-dl
  (:use #:cl
        #:drakma
        #:cl-ppcre
        #:html-parse)
  
  (:export #:download
           #:batch-download))

(declaim (optimize (safety 3)) (optimize (debug 3)) (optimize (speed 0)))
(declaim (inline get-image-id get-href get-resolution prompt-read))