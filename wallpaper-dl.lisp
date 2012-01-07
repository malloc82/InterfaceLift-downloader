(ql:quickload :drakma)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-html-parse)

(defpackage :wallpaper-dl (:use :cl :drakma :cl-ppcre :html-parse))
(in-package :wallpaper-dl)

(defvar *base-url*  "http://interfacelift.com/wallpaper/downloads/date/widescreen/")
(defvar *site* "http://interfacelift.com")
(defvar *last-record* ".last")

(defmacro print-asap (control-string &rest args)
  `(progn
     (format *query-io* ,control-string ,@args)
     (force-output *query-io*)))

(defun prompt-read (prompt)
  (print-asap "~a: " prompt)
  (read-line *query-io*))

(defun delay (seconds &key (message-before nil) (message-after nil) (newline t))
  (macrolet ((display-message (message)
               `(when (and ,message (stringp ,message))
                  (print-asap ,message))))
    (display-message message-before)
    (loop for sec from 1 to seconds do
         (progn
           (sleep 1)
           (print-asap ".")))
    (display-message message-after)
    (when newline
      (print-asap "~%"))))


(defun get-all-images-with-resolution (&key
                                       (resolution "1440x900")
                                       (url nil)
                                       (download-dir resolution)
                                       (auto nil)
                                       (pages-to-download nil)
                                       (last-download nil)
                                       (record nil))
  (when (and url
             (or (null pages-to-download)
                 (> pages-to-download 0)))
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
                               (print-asap "Download successful: ~a" filename)
                               (when record
                                   (with-open-file (save (concatenate 'string download-dir *last-record*)
                                                         :direction :output
                                                         :if-does-not-exist :create
                                                         :if-exists :supersede)
                                     (write-sequence image-name save)))
                               (delay 3 :message-after "done")) ;; 
                             (progn
                               (print-asap "Something went wrong ... link: ~a~%" link)
                               (print-asap "                         length: ~d~%" (length data))))))))))
          (print-asap "~%Finished page ~a ~%~%" url)
          (when next-page-html
            (let ((next-page-link (concatenate 'string
                                               *site*
                                               (get-href (html-parse:parse-html next-page-html)))))
              (print-asap "Next page: ~a~%" next-page-link)
              (when (or auto (y-or-n-p "Continue?"))
                (delay 5 :message-after "OK")
                (print-asap "------------------------------------------------------~%")
                (when pages-to-download
                  (decf pages-to-download)
                  (if (> pages-to-download 0)
                      (print-asap "There are ~d pages to go.~%" pages-to-download)
                      (print-asap "All pages are downloaded.~%")))
                (get-all-images-with-resolution :resolution resolution
                                                :url next-page-link
                                                :last-download last-download
                                                :download-dir download-dir
                                                :auto auto
                                                :pages-to-download pages-to-download)))))))))

(defun download-main (&key
                      (resolutions nil)
                      (pages 1))
  (unless resolutions
    (setq resolutions
          (prompt-read "Enter wallpaer resolutions (use ',' to seperate different resolutions)")))
  (mapcar #'(lambda (resolution)
              (let ((resolution-path (make-pathname :directory `(:relative "test" ,resolution)))
                    (page-url (concatenate 'string *base-url* resolution "/")))
                (ensure-directories-exist resolution-path)
                (print-asap "Start downloading resolution ~a from ~a~%" resolution page-url)
                (get-all-images-with-resolution :resolution resolution
                                                :url page-url
                                                :download-dir (directory-namestring resolution-path)
                                                :auto t
                                                :pages-to-download pages)))
          (cl-ppcre:split "\\s*,\\s*" resolutions)))

