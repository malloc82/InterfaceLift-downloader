(ns interfacelift-dl.core
  (:use clojure.core
        [clojure.pprint :only [pprint]])
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as sel]))

(set! *warn-on-reflection* true)

(def header {:user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4_Beta) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"})

;; ASUS monitor
(def ^String res_1920x1080  "https://interfacelift.com/wallpaper/downloads/date/wide_16:9/1920x1080/")
;; retina MBP
(def ^String res_3360x2100  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/3360x2100/")
(def ^String res_2880x1800  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/2880x1800/")
(def ^String res_1680x1050  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/1680x1050/")

(def ^String base-url  "https://interfacelift.com")
(def ^String query-url "https://interfacelift.com/wallpaper/downloads/date" )

(def response (client/get test-url {:headers header}))
(def body (hickory/as-hickory (hickory/parse (:body response))))

(def dl-folder "resources")

(defn trim-url [^String url]
  (loop [index (.length url)]
    (if (zero? index)
      ""
      (let [c (.charAt url (unchecked-dec index))]
       (if (or (= c \/) (Character/isWhitespace c))
         (recur (unchecked-dec index))
         #_(.subSequence s 0 index)
         (.. url (subSequence 0 index) toString))))))

(defn getPages [body]
  (set (mapv #(:href (:attrs %))
             (sel/select (sel/class "selector") body))))

(defn getImageLinks [body]
  (mapv (comp #(:href (:attrs (first %)))
              #(sel/select (sel/attr "href") %))
        (sel/select (sel/class "download") body)))

(defn write-image [^String f img-array]
  (println "--------> " (class img-array))
  (with-open [w (java.io.BufferedOutputStream. (java.io.FileOutputStream. f))]
    (.write w img-array)))

(defn download-image [url ^String f resp]
  (clojure.java.io/copy
   (:body (client/get url {:header header :as :stream :cookies (:cookies resp)}))
   (java.io.File. f)))

(defn getImageID [^String url]
  (first (clojure.string/split (.getName (java.io.File. url)) #"_")))

(defn getImageResolution
  "Get the image resolution based on image url.
   img-src has to be a direct image url."
  [img-src]
  (first (take-last 2 (clojure.string/split img-src #"[/_.]"))))

(defn getResolution
  "Get resolution based on page url"
  [^String page-url]
  (let [ext (last (clojure.string/split page-url #"[.]"))]
    (if (#{"html"} ext)
      (first (take-last 2 (clojure.string/split page-url #"[/]")))
      (first (clojure.string/split (.getName (java.io.File. page-url)) #"_")))))

(defn bulk-download-page [page-url & {:keys [delay  debug]
                                 :or   {delay  10000
                                        debug  false}}]
  (let [resp     (client/get page-url {:headers header})
        body     (hickory/as-hickory (hickory/parse (:body resp)))
        img-list (getImageLinks body)
        res      (getResolution page-url)
        folder   (let [base "resources/"]
                   (if (or (empty? res) (nil? res))
                     base
                     (str base res)))]
    ;; (pprint img-list)
    (.mkdir (java.io.File. folder))
    (doseq [link img-list]
      (let [f (java.io.File. ^String link)
            im_name ^String (.getName f)
            img-src (clojure.string/join [base-url link])
            target-path (java.io.File. (clojure.string/join [folder "/" im_name]))]
        (when debug
          (println "    ==> f" link)
          (println "    ==> img-src" img-src))
        (print "Downloading" img-src "...") (flush)
        (if (.exists target-path)
          (do (println " File exists, skipping.") (flush))
          (let [dl-resp (client/get img-src {:header header :as :stream :cookies (:cookies resp)})]
            (clojure.java.io/copy (:body dl-resp) target-path)
            (println " Done. Saved to: " (str folder "/" im_name)) (flush)
            (Thread/sleep delay)))))))

(defn download-multi-pages
  "base-url should be a url without index.html
   page-range should be a list of integers"
  [base-url page-list]
  (doseq [page page-list]
    (let [page-url (format "%s/index%d.html" (trim-url base-url) page)]
      (println "Download page" page-url)
      (bulk-download-page page-url))))

;; (download-multi-pages res_1920x1080 (range 1 10))
;; (download-multi-pages res_3360x2100 (range 1 10))
