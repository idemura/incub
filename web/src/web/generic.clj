(ns web.generic
  (:require
    [clojure.string :as str])
  (:import
    [java.net URLEncoder]
    [clojure.lang IPersistentMap IPersistentVector]))

(defprotocol IUrlEncodable
  (^String url-encode-value [v]))

(extend-protocol IUrlEncodable
  String
  (url-encode-value [v]
    (-> v URLEncoder/encode (.replace "+" "%20")))
  IPersistentVector
  (url-encode-value [v]
    (->> v (map url-encode-value) (str/join "+"))))

(defn ^String url-encode
  [params]
  (letfn [(encode [[k v]]
            (str (name k) "=" (url-encode-value v)))]
    (->> params (map encode) (interpose "&") str/join)))

(defn ^String url-encode-request
  [host params]
  (if (empty? params)
    host
    (str host "?" (url-encode params))))
