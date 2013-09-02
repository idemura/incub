(defproject con_clj "0.1.0-SNAPSHOT"
  :description "Experiment project to learn the Clojure language"
  :url "http://github.com/idemura/incub"
  :license {:name "Eclipse Public License",
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
    [[org.clojure/clojure "1.5.1"]
     [org.clojure/clojure-contrib "1.2.0"]]
  :main con-clj.test
  :global-vars { #_(*warn-on-reflection* true) })
