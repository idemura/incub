(ns web.handler
  (:require
    [clojure.string :refer [join]]
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html])
  (:import
    [java.net URLEncoder]
    [clojure.lang IPersistentVector IPersistentMap]))

(defprotocol IUrlEncodable
  (^String url-encode-value [v]))

(extend-protocol IUrlEncodable
  String
  (^String url-encode-value [v]
    (-> v URLEncoder/encode (.replace "+" "%20")))
  IPersistentVector
  (^String url-encode-value [v]
    (->> v (map url-encode-value) (join "+"))))

(defn ^String url-encode-query
  [params]
  (letfn [(encode [[k v]]
            (str (name k) "=" (url-encode-value v)))]
    (->> params (map encode) (interpose "&") join)))

(defn ^String url-encode
  [host params]
  (if (empty? params)
    host
    (str host "?" (url-encode-query params))))

(def ^:const CLIENT_ID "484563975237.apps.googleusercontent.com")
(def ^:const CLIENT_SECRET "XyjfDwvcQUA8xYO9n9iW0iW2")
(def ^:const CLIENT_SCOPES
  ["https://www.googleapis.com/auth/userinfo.email"
   "https://www.googleapis.com/auth/userinfo.profile"])

(def oauth2-uri (url-encode
                  "https://accounts.google.com/o/oauth2/auth"
                  {:response_type "code"
                   :access_type "online"
                   :redirect_uri "http://localhost:3000/oauth2callback"
                   :client_id CLIENT_ID
                   :scope CLIENT_SCOPES}))

(html/deftemplate view-index
  {:parser html/xml-parser} "templates/index.html"
  []
  [:a#LoginWithGoogle] (html/set-attr :href oauth2-uri))

(defn handle-index
  [request]
  ; (println request)
  {:headers {"idemura-custom", "value"}
   :body    (do (println oauth2-uri)
                (apply str (view-index)))})

(defn handle-oauth2-callback
  [request]
  (println (request :request-method))
  "Zzzzzzzzz")

(defroutes app-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  (ANY "/oauth2callback" [] handle-oauth2-callback)
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
