(defproject exp "0.1.0"
  :description "Experiment project to learn the Clojure"
  :url "http://github.com/idemura/incub"
  :license {
    :name "Eclipse Public License",
    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  ; :aot [exp.test]
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [org.clojure/clojure-contrib "1.2.0"]
    [org.clojure/data.json "0.2.3"]]
  :main exp.test
  :global-vars {*warn-on-reflection* false})
