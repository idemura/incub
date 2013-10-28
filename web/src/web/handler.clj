(ns web.handler
  (:use web.data web.generic web.view)
  (:require
    [clojure.data.json :as json]
    [compojure.core :refer :all]
    [compojure.handler]
    [compojure.route :as route])
  (:import
    [java.net HttpURLConnection URL]))

(def ^:const CLIENT_ID "484563975237.apps.googleusercontent.com")
(def ^:const CLIENT_SECRET "XyjfDwvcQUA8xYO9n9iW0iW2")
(def ^:const CLIENT_SCOPES
  ["https://www.googleapis.com/auth/userinfo.email"
   "https://www.googleapis.com/auth/userinfo.profile"])
(def ^:const ERROR_GAUTH "Error authenticating with Google.")
(def ^:const ERROR_FETCH_USERINFO "Error fetching user info.")

(def ^:private oauth2_uri
  (url-encode-request
    "https://accounts.google.com/o/oauth2/auth"
    {:response_type "code"
     :access_type "online"
     :redirect_uri "http://localhost:3000/oauth2"
     :client_id CLIENT_ID
     :scope CLIENT_SCOPES}))

(defn handle-index
  [request]
  {:headers {"idemura-custom", "value"}
   :body    (apply str (view-index oauth2_uri))})

(defn ^:private exchange-for-token
  [code]
  (let [form (url-encode {
                :code code
                :client_id CLIENT_ID
                :client_secret CLIENT_SECRET
                :redirect_uri "http://localhost:3000/oauth2"
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
         :data (json/write-str {"error" (.getMessage e)})}))))

;; IN: access_token: String
;; OUT: {String String} or nil.
(defn ^:private get-userinfo
  [access_token]
  (let [url "https://www.googleapis.com/oauth2/v2/userinfo"
        params {:alt "json" :access_token "access_token"}]
    (->> (url-encode-request url params) slurp json/read-str)))

(defn ^:private access-granted
  [cred]
  (let [userinfo (cred "access_token")]
    (save-user userinfo)
    (view-error (str "It's OK, Houston " (userinfo "name")))))

(defn handle-oauth2
  [request]
  (let [params (request :query-params)
        oauth2-code (params "code")
        {code :code data :data} (exchange-for-token oauth2-code)]
    (if data
      (access-granted (json/read-str data))
      (view-error (if code
                    (str "Error: HTTP code " code ": "
                         ((json/read-str data) "error"))
                    (view-error ERROR_GAUTH))))))

(defn handle-echo
  [request]
  (str request))

(defroutes my-routes
  (GET "/" [] handle-index)
  (GET "/ping/:what" [what] (str "<h1>Ping " what "</h1>"))
  ; This path is registered in the Google API console.
  (GET "/oauth2" [] handle-oauth2)
  (ANY "/echo" [] handle-echo)
  (route/resources "/")
  (route/not-found "Not Found"))

;; Called before handlers begin their work.
(defn startup
  []
  (configure-db))

(def handler
  (compojure.handler/site my-routes))
