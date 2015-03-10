(defproject pgenerator "0.1.0-SNAPSHOT"
  :description "Simple pattern-generating implementation based on graphs-approach"
  :url "https://github.com/aplatonovv/inhound"
  :license {:name "GNU GENERAL PUBLIC LICENSE"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot pgenerator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
