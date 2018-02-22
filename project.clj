(defproject wam "0.1.0-SNAPSHOT"
  :description "Tutorial implementation of the Warren Abstract Machine"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha20"]
                 [instaparse "1.4.8"]]
  :main ^:skip-aot wam.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[cider/cider-nrepl "0.15.1" :exclusions [org.clojure/tools.nrepl]]]}})
