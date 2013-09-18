(ns web.handler
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html]))

(html/deftemplate view-index
  {:parser html/xml-parser} "templates/index.html"
  []
  [:a#LoginWithGoogle] (html/set-attr :href "hello"))

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
