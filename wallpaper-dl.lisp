(ql:quickload :drakma)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-html-parse)

(defpackage :wallpaper-dl (:use :cl :drakma :cl-ppcre :html-parse))
(in-package :wallpaper-dl)

(defvar *base-url*  "http://interfacelift.com/wallpaper/downloads/date/widescreen/")
(defvar *site* "http://interfacelift.com")
(defvar *last-record* ".last")

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun delay (seconds &key (message nil) (newline t))
  (force-output *query-io*)
  (loop for sec from 1 to seconds do
       (progn
         (sleep 1)
         (format *query-io* ".")
         (force-output *query-io*)))
  (when (and message (stringp message))
    (if newline
        (format *query-io* "~a~%" message)
        (format *query-io* message))
    (force-output *query-io*)))

(defmacro output-w-flush (control-string &rest args)
  `(progn
     (format *query-io* ,control-string ,@args)
     (force-ouput *query-io*)))

(defun get-all-images-with-resolution (&key
                                       (resolution "1440x900")
                                       (url nil)
                                       (last-download nil)
                                       (download-dir resolution)
                                       (auto nil)
                                       (record nil))
  (when record
    )
  (when url
    (macrolet ((get-href (html-entry)
                 `(getf (cdr (first (first ,html-entry))) :href)))
      (let* ((html-string (http-request url :user-agent "Mozilla"))
             (image-links (cl-ppcre:all-matches-as-strings
                           (format nil "[^=<>\"]+~a.jpg" resolution) html-string))
             (next-page-html (first (cl-ppcre:all-matches-as-strings
                                     "<a class=\"selector\".+next page ></a>" html-string))))
        (when image-links
          (loop for link in image-links do
               (let ((image-name (file-namestring link)))
                 (if (string= last-download image-name)
                     (setq next-page-html nil)
                     (let ((filename (concatenate 'string download-dir image-name))
                           (link (concatenate 'string *site* link)))
                       
                       (with-open-file (file-stream filename
                                                    :direction :output
                                                    :if-does-not-exist :create
                                                    :if-exists :supersede
                                                    :element-type '(unsigned-byte 8))
                         (let ((data (http-request link :user-agent "Mozilla")))
                         (if (write-sequence  data file-stream)
                             (progn
                               (format *query-io* "Download successful: ~a" filename)
                               (when record
                                   (with-open-file (save (concatenate 'string download-dir *last-record*)
                                                         :direction :output
                                                         :if-does-not-exist :create
                                                         :if-exist :supersede)
                                     (write-sequence image-name save)))
                               (delay 3 :message "done")) ;; 
                             (progn
                               (format t "Something went wrong ... link: ~a~%" link)
                               (format t "                         length: ~d~%" (length data))))))))))
          (format t "~%Finished page ~a ~%~%" url)
          (when next-page-html
            (let ((next-page-link (concatenate 'string
                                               *site*
                                               (get-href (html-parse:parse-html next-page-html)))))
              (format t "Next page: ~a~%" next-page-link)
              (when (or auto (y-or-n-p "Continue?"))
                (delay 5 :message "OK")
                (format t "------------------------------------------------------~%")
                (get-all-images-with-resolution :resolution resolution
                                                :url next-page-link
                                                :last-download last-download
                                                :download-dir download-dir)))))))))

(defun download-main (&optional (resolutions nil))
  (unless resolutions
    (setq resolutions
          (prompt-read "Enter wallpaer resolutions (use ',' to seperate different resolutions)")))
  (mapcar #'(lambda (resolution)
              (let ((resolution-path (make-pathname :directory `(:relative "test" ,resolution)))
                    (page-url (concatenate 'string *base-url* resolution "/")))
                (ensure-directories-exist resolution-path)
                (format t "Start downloading resolution ~a from ~a~%" resolution page-url)
                (get-all-images-with-resolution :resolution resolution
                                                :url page-url
                                                :download-dir (directory-namestring resolution-path))))
          (cl-ppcre:split "\\s*,\\s*" resolutions)))

