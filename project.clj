(defproject interfacelift-dl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "3.1.0"]
                 [hickory "0.6.0"]]
  ;; :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace []
  :repl-options {:port 7000})
