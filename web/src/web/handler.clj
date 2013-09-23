(ns web.handler
  (:use web.generic)
  (:require
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route]
    [net.cgrand.enlive-html :as html])
  (:import
    [clojure.lang IPersistentMap IPersistentVector]
    [java.net HttpURLConnection URL]))

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
  {:headers {"idemura-custom", "value"}
   :body    (apply str (view-index))})

(defn- exchange-for-token
  [code]
  (let [form (url-encode
               {:code code
                :client_id CLIENT_ID
                :client_secret CLIENT_SECRET
                :redirect_uri "http://localhost:3000/oauth2/code"
                :grant_type "authorization_code"})
        addr "https://accounts.google.com/o/oauth2/token"
        conn (let [^HttpURLConnection conn (.openConnection (URL. addr))]
               (doto conn
                 (.setDoOutput true)
                 (.setRequestProperty "Content-Type" MIME_FORM_URLENCODED)
                 (.setRequestProperty "Content-Length" (str (count form)))))]
    (try
      (with-open [os (.getOutputStream conn)]
        (.write os (.getBytes form)))
      (let [code (.getResponseCode conn)]
        {:code code
         :data (slurp (if (= code 200)
                        (.getInputStream conn)
                        (.getErrorStream conn)))})
      (catch Exception e
        {:code 400
         :data (.getMessage e)}))))

(defn handle-oauth2-code
  [request]
  (let [params (request :query-params)
        param-code (params "code")]
    (if param-code
      (let [{code :code data :data} (exchange-for-token param-code)]
        (if (= code 200)
          (view-error (str "success " data))
          (view-error (str "failure " code " :: " data))))
      (view-error ERROR_GAUTH))))

(defn handle-echo
  [request]
  (println request)
  "ECHO")

(defroutes app-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  ;; This path is registered in the Google API console.
  (GET "/oauth2/code" [] handle-oauth2-code)
  (ANY "/echo" [] handle-echo)
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (compojure.handler/site app-routes))
