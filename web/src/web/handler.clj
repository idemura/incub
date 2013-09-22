(ns web.handler
  (:use web.generic)
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html])
  (:import
    [java.net HttpURLConnection URL]
    [clojure.lang IPersistentMap IPersistentVector]))

(def ^:const CLIENT_ID "484563975237.apps.googleusercontent.com")
(def ^:const CLIENT_SECRET "XyjfDwvcQUA8xYO9n9iW0iW2")
(def ^:const CLIENT_SCOPES
  ["https://www.googleapis.com/auth/userinfo.email"
   "https://www.googleapis.com/auth/userinfo.profile"])
(def ^:const ERROR_GAUTH "Error authenticating with Google.")

(def oauth2-uri (url-encode-request
                  "https://accounts.google.com/o/oauth2/auth"
                  {:response_type "code"
                   :access_type "online"
                   :redirect_uri "http://localhost:3000/oauth2/code"
                   :client_id CLIENT_ID
                   :scope CLIENT_SCOPES}))

(html/deftemplate view-index
  {:parser html/xml-parser} "templates/index.html"
  []
  [:#GoogleOauth2Login] (html/set-attr :href oauth2-uri))

(html/deftemplate view-error
  {:parser html/xml-parser} "templates/error.html"
  [msg]
  [:#ErrorMessage] (html/content msg))

(defn handle-index
  [request]
  ; (println request)
  {:headers {"idemura-custom", "value"}
   :body    (do (println oauth2-uri)
                (apply str (view-index)))})

(defmacro catch-all
  ([forms]
    `(catch Exception ~@forms)))

(defn- write-output
  [^HttpURLConnection conn ^String data]
  (try
    (let [st (.getOutputStream conn)]
      (try
        (.write st (.getBytes form))
        conn
      (finally
        (.close st))))
    (catch-all conn)))

(defn- exchange-for-token
  [code]
  (let [form (url-encode {:code code
                          :client_id CLIENT_ID
                          :client_secret CLIENT_SECRET
                          :redirect_uri "http://localhost:3000/oauth2/token"
                          :grant_type "authorization_code"})
        addr "https://accounts.google.com/o/oauth2/token"
        conn (doto
               (.openConnection (URL. addr))
               (.setDoOutput true)  ;; Triggers POST.
               (.setRequestProperty "Content-Type" "application/x-www-form-urlencoded; charset=utf-8")
               (.setRequestProperty "Content-Length" (count form)))
        ]
    (write-output conn)
    (let [is (.getInputStream conn)]
      )))

(defn handle-oauth2-code
  [request]
  (let [params (request :query-params)
        param-code (params "code")]
    (if param-code
      (exchange-for-token param-code)
      (view-error ERROR_GAUTH))))

(defroutes app-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  ;; This name is registered in Google API console.
  (ANY "/oauth2callback" [] handle-oauth2-code)
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
