(ns web.handler
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html]))

(defroutes app-routes
  (GET "/" [] "<p>Hello <b>World</b></p>")
  (GET "/ping/:what" [what] (str "<h1>Ping '" what "'</h1>"))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
