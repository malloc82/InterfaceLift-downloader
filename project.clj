(defproject interfacelift-dl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-http "3.9.1"]
                 [hickory "0.7.1"]
                 [org.clojure/tools.cli "0.3.7"]]
  ;; :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace []
  :repl-options {:port 7000}
  :aot [interfacelift-dl.core]
  :main interfacelift-dl.core)
