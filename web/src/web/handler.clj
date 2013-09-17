(ns web.handler
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html]))

;; Can I insert it into ns decl?
(html/set-ns-parser! html/xml-parser)

(html/deftemplate index "templates/index"
  [])

(defroutes app-routes
  (GET "/" [] (index))
  (GET "/ping/:what" [what] (str "<h1>Ping '" what "'</h1>"))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
