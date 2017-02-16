(in-package #:wallpaper-dl)

(defvar *base-url*  "http://interfacelift.com/wallpaper/downloads/date/widescreen/")
(defvar *site* "http://interfacelift.com")
(defvar *last-visit* nil)
;; (defvar *last-record* "~/.")

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

(defun get-image-id (imagename)
  (first (cl-ppcre:split "_" (file-namestring imagename))))

(defun get-href (html-entry)
  (getf (cdr (first (first html-entry))) :href))

(defun get-resolution (url)
  (first (cl-ppcre:all-matches-as-strings "\\d+x\\d+" url)))

(defun extract-links (url &key (resolution nil))
  (let* ((resolution     (if resolution resolution (get-resolution url)))
         (html-string    (http-request url :user-agent "Mozilla"))
         (image-links    (mapcar #'(lambda (link)
                                     (concatenate 'string *site* link))
                                 (cl-ppcre:all-matches-as-strings
                                  (format nil "[^=<>\"]+~a.jpg" resolution) html-string)))
         (next-page-html (first (cl-ppcre:all-matches-as-strings
                                     "<a class=\"selector\".+next page ></a>" html-string)))
         (prev-page-html (first (cl-ppcre:all-matches-as-strings
                                     "<a class=\"selector\".+< prev page</a>" html-string))))
    (values image-links
            (if next-page-html
                (concatenate 'string *site* (get-href (html-parse:parse-html next-page-html)))
                nil)
            (if prev-page-html
                (concatenate 'string *site* (get-href (html-parse:parse-html prev-page-html)))
                nil))))

(defun download-image (link dir)
  (destructuring-bind (name type)
      (cl-ppcre:split "\\." (file-namestring link))
    (let* ((filename  (make-pathname :directory `(:relative ,dir) :name name :type type)))
      (with-open-file (file-stream (namestring (ensure-directories-exist filename))
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
        (let ((data (http-request link :user-agent "Mozilla")))
          (if (write-sequence  data file-stream)
              (progn
                (print-asap "Download successful: ~a" (file-namestring filename))
                ;; (when record
                ;;     (with-open-file (save (concatenate 'string download-dir *last-record*)
                ;;                           :direction :output
                ;;                           :if-does-not-exist :create
                ;;                           :if-exists :supersede)
                ;;       (write-sequence image-name save)))
                (delay 2 :message-after "done")
                (file-namestring filename)) ;; 
              (progn
                (print-asap "Something went wrong ... link: ~a~%" link)
                (print-asap "                         length: ~d~%" (length data))
                nil)))))))

(defun proc-page (page-link)
  (let* ((links (extract-links page-link))
         (filename (download-image (first links) "temp")))
    filename))

(defun search-image (image-id start-url &key (end nil))
  (print-asap "Searching page : ~a~%" start-url)
  (multiple-value-bind (image-links next-page prev-page)
      (extract-links start-url)
    (loop
       :initially (if (string> image-id (get-image-id (first image-links)))
                      (progn
                        (if prev-page
                            (search-image image-id prev-page)
                            (return (values nil nil nil)))))
       :for link :in image-links
       :do (cond ((string= image-id (get-image-id (file-namestring link)))
                  (return (values image-links next-page prev-page)))
                 ((and end (string<= image-id end))
                  (return (values nil nil nil)))
                 (t (pop image-links)))
       :finally (return
                  (if next-page (search-image image-id next-page) (values nil nil nil))))))

(defun resume ()
  (when *last-visit*
    (apply #'batch-download *last-visit*)))

(defun batch-download (page-url dir &key (first nil) (last nil))
  (setq *last-visit* (list page-url dir :first first :last last))
  (multiple-value-bind (image-links next-page)
      (if first
          (search-image first page-url)
          (extract-links page-url))
    (loop
       :for link :in image-links
       :do  (if (and last (string< (get-image-id link) last))
                (return nil)
                (download-image link dir))
       :finally (when next-page (batch-download next-page dir :last last)))))

(defun download (&key (resolutions nil) (directory "wallpaper_download") (first nil) (last nil))
  (unless resolutions
    (setq resolutions
          (prompt-read "Enter wallpaer resolutions (use ',' to seperate different resolutions)")))
  (mapcar #'(lambda (resolution)
              (let ((resolution-path
                     (make-pathname :directory `(:relative ,directory ,resolution)))
                    (page-url (concatenate 'string *base-url* resolution "/")))
                (ensure-directories-exist resolution-path)
                (print-asap "Start downloading resolution ~a from ~a~%" resolution page-url)
                (batch-download page-url (directory-namestring resolution-path)
                                :first first
                                :last last)))
          (cl-ppcre:split "\\s*,\\s*" resolutions)))
