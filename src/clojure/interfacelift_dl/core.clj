(ns interfacelift-dl.core
  (:use clojure.core
        [clojure.pprint :only [pprint]])
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as sel]
            [clojure.tools.cli :refer [parse-opts]])
  (:import [java.nio.file Paths])
  (:gen-class))

(set! *warn-on-reflection* true)

(def header {:user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4_Beta) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"})

;; ASUS monitor
(def ^String res_1920x1080  "https://interfacelift.com/wallpaper/downloads/date/wide_16:9/1920x1080/")
;; retina MBP
(def ^String res_3360x2100  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/3360x2100/")
(def ^String res_2880x1800  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/2880x1800/")
(def ^String res_1680x1050  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/1680x1050/")
;; DELL U2417H setting
(def ^String res_1080x1920  "https://interfacelift.com/wallpaper/downloads/date/wide_9:16/1080x1920/")

(def res {"1920x1080"  "https://interfacelift.com/wallpaper/downloads/date/wide_16:9/1920x1080/"
          "3360x2100"  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/3360x2100/"
          "2880x1800"  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/2880x1800/"
          "1680x1050"  "https://interfacelift.com/wallpaper/downloads/date/wide_16:10/1680x1050/"
          "1080x1920"  "https://interfacelift.com/wallpaper/downloads/date/wide_9:16/1080x1920/"})

(def ^String base-url  "https://interfacelift.com")
(def ^String query-url "https://interfacelift.com/wallpaper/downloads/date" )

;; (def response (client/get test-url {:headers header}))
;; (def body (hickory/as-hickory (hickory/parse (:body response))))

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

(defn write-image [^String f ^bytes img-array]
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
  ;; (let [ext (last (clojure.string/split page-url #"[.]"))]
  ;;   (if (#{"html"} ext)
  ;;     (first (take-last 2 (clojure.string/split page-url #"[/]")))
  ;;     (first (clojure.string/split (.getName (java.io.File. page-url)) #"_"))))
  (or (re-find #"[0-9]+x[0-9]+" page-url) "Unknown"))

(defn bulk-download-page [page-url & {:keys [delay debug folder]
                                      :or   {delay  10000
                                             debug  false
                                             folder "resources"}}]

  (let [resp     (client/get page-url {:headers header :insecure? true})
        body     (hickory/as-hickory (hickory/parse (:body resp)))
        img-list (getImageLinks body)
        ;; img-list (->> (client/get res_1920x1080 {:header header})
        ;;               :body
        ;;               hickory/parse
        ;;               hickory/as-hickory
        ;;               getImageLinks)
        res      (getResolution page-url)
        folder   (-> (Paths/get folder (into-array [res]))
                     str) ;; varargs java methods need an array containing
                                                       ;; all remaining args as their final argument.
                 ;; (let [base "resources/"]
                 ;;   (if (or (empty? res) (nil? res))
                 ;;     base
                 ;;     (str base res)))
        ]
    ;; (pprint img-list)
    (.mkdirs (java.io.File. folder))
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
          (let [dl-resp (client/get img-src {:header header :as :stream :cookies (:cookies resp) :insecure? true})]
            (clojure.java.io/copy (:body dl-resp) target-path)
            (println " Done. Saved to: " (str folder "/" im_name)) (flush)
            (Thread/sleep delay)))))))

(defn download-multi-pages
  "base-url should be a url without index.html
   page-range should be a list of integers"
  [base-url & {:keys [page-list folder]
               :or   {page-list '(1)
                      folder "resources/"}}]
  (doseq [page page-list]
    (let [page-url (format "%s/index%d.html" (trim-url base-url) page)]
      (println "Download page" page-url)
      (bulk-download-page page-url :folder folder))))
;; (download-multi-pages res_1920x1080 "./resources" :page-list (range 1 10))
;; (download-multi-pages res_3360x2100 "./resources" :page-list (range 1 10))

(def cli-options
  [[nil "--res RES" "Resolution"
    :id :res
    :default "1920x1080"
    :parse-fn str]
   ["-d" "--folder FOLDER" "Download folder"
    :id :folder
    :default "resources"
    :parse-fn str]
   ["-r" "--range RANGE" "Pages to download"
    :id :range
    :default '(1)
    :parse-fn #(eval (read-string %))]
   ["-h" "--help" "Print help page"
    :id :help
    :default false]])

(defn usage [options-summary]
  (->> ["This program downloads wallpapers with desired resolution from interfacelift.com."
        ""
        "Usage: IFL-DL [options]"
        ""
        "Options:"
        options-summary
        "\nExample:"
        "        clj -M:main --res 2880x1800 -r \"(range 10)\""
        "        clj -M:main --res 2880x1800 -r \"'(1 2 3)\""]
       (clojure.string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      :else
      {:options options} )))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (pprint args)
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (download-multi-pages (get res (:res options)) :folder (:folder options) :page-list (:range options)))))
