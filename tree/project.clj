(defproject con_clj "0.1.0"
  :description "Tree console utility in Clojure"
  :url "http://github.com/idemura/incub"
  :license {
    :name "Eclipse Public License",
    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [org.clojure/clojure-contrib "1.2.0"]]
  :main tree.main
  :global-vars { #_(*warn-on-reflection* true) }
  :profiles {
    :opt {
      :debug false
      :javac-options ["-target" "1.6" "-source" "1.6" "-g:none"]}})
