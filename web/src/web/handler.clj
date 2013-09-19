(ns web.handler
  (:require
    [clojure.string :refer [join]]
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html])
  (:import
    [java.net URLEncoder]))

(html/deftemplate view-index
  {:parser html/xml-parser} "templates/index.html"
  []
  [:a#LoginWithGoogle] (html/set-attr :href "hello"))

(defn handle-index
  [request]
  ; (println request)
  {:headers {"idemura-custom", "value"}
   :body    (apply str (view-index))})

(defn handle-oauth2-callback
  [request]
  (println (request :request-method))
  "Zzzzzzzzz")

(defn url-encode-query
  [params]
  (letfn [(encode [[k v]]
            (str (name k) "=" (-> v URLEncoder/encode (.replace "+" "%20"))))]
    (->> params (map encode) (interpose "&") join str)))

(defn url-encode
  [host params]
  (if (empty? params)
    host
    (str host "?" (url-encode-query params))))

(def ^:const CLIENT_ID "484563975237.apps.googleusercontent.com")
(def ^:const CLIENT_SECRET "XyjfDwvcQUA8xYO9n9iW0iW2")
(def oauth2-uri (url-encode
                  "https://accounts.google.com/o/oauth2/auth"
                  {:redirect_uri "localhost:3000/oauth2callback"
                   :client_id CLIENT_ID}))

(defroutes app-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  (ANY "/oauth2callback" [] handle-oauth2-callback)
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
