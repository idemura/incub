(defproject web "0.1.0"
  :description "Compojre Web App"
  :url "http://github.com/idemura/incub"
  :license {
    :name "Eclipse Public License",
    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [org.clojure/data.json "0.2.3"]
    [compojure "1.1.5"]
    [korma "0.3.0-RC6"]
    [enlive "1.1.1"]
    [com.h2database/h2 "1.3.170"]
    [log4j "1.2.15" :exclusions [
        javax.mail/mail
        javax.jms/jms
        com.sun.jdmk/jmxtools
        com.sun.jmx/jmxri]]]
  :plugins [[lein-ring "0.8.5"]]
  :ring {
    :handler web.handler/handler
    :init web.handler/startup}
  :main web.sandbox
  :global-vars {*warn-on-reflection* true}
  :profiles {
    :dev {
      :dependencies [[ring-mock "0.1.5"]]}
    :opt {
      :debug false
      :javac-options ["-target" "1.6" "-source" "1.6" "-g:none"]}})
