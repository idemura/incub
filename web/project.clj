(defproject web "0.1.0"
  :description "Compojre Web App"
  :url "http://github.com/idemura/incub"
  :license {
    :name "Eclipse Public License",
    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [compojure "1.1.5"]
    [enlive "1.1.1"]]
  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler web.handler/app}
  :global-vars {#_(*warn-on-reflection* true)}
  :profiles {
    :dev {
      :dependencies [[ring-mock "0.1.5"]]}
    :opt {
      :debug false
      :javac-options ["-target" "1.6" "-source" "1.6" "-g:none"]}})
