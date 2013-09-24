(ns web.generic
  (:require
    [clojure.string :as str])
  (:import
    [java.net URLEncoder]
    [clojure.lang IPersistentMap IPersistentVector]))

(def ^:const MIME_FORM_URLENCODED
    "application/x-www-form-urlencoded; charset=utf-8")

(defprotocol IUrlEncodable
  (^String url-encode [v]))

(extend-protocol IUrlEncodable
  String
  (url-encode [v]
    (-> v URLEncoder/encode (.replace "+" "%20")))
  IPersistentVector
  (url-encode [v]
    (->> v (map url-encode) (str/join "+")))
  IPersistentMap
  (url-encode [v]
    (letfn [(encode [[k v]]
              (str (name k) "=" (url-encode v)))]
      (->> v (map encode) (interpose "&") str/join))))

(defn ^String url-encode-request
  [host params]
  (if (empty? params)
    host
    (str host "?" (url-encode params))))