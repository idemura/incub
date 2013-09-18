(ns web.handler
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html]))

(html/set-ns-parser! html/xml-parser)

(html/deftemplate view-index "templates/index.html"
  [])

(defn handle-index
  [request]
  ; (println request)
  {:headers {"idemura-custom", "value"}
   :body    (apply str (view-index))})

(defroutes app-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
