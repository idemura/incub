(ns web.data
  (:use web.generic)
  (:require
    [korma.config]
    [korma.db]
    [korma.core :as ql]))

(korma.db/defdb db-spec
  (korma.db/h2 {:db "tcp://localhost:9092/~/local"
                :user "sa"
                :password ""}))

(ql/defentity accounts (ql/table :accounts))
(def ^:private account_fields
  #{:email :gender :name :given_name :birthday :picture :local})

(defn configure-db
  []
  (korma.config/set-delimiters ""))

(defn ^:private lower
  [m]
  (letfn [(lc [^String s] (.toLowerCase s))]
    (map-map #(-> % name lc keyword) m)))

;; OUT (List (Map Keyword String)): list of found emails. Length 0 or 1.
(defn- accounts-find-email
  [email]
  (map lower (ql/select accounts (ql/where {:email email}))))

(defn- accounts-insert
  [values]
  (let [vs (map-filter account_fields values)]
    (ql/insert accounts (ql/values vs))))

(defn- accounts-update
  [values id]
  (let [vs (map-filter account_fields values)]
    (ql/update accounts (ql/set-fields vs) (ql/where {:id id}))))

;; IN user (Map Keyword String): of columns to insert.
;; OUT nil
(defn save-user
  [user]
  (configure-db)
  (korma.db/transaction
    (let [us (accounts-find-email (:email user))]
      (if (= (count us) 0)
        (accounts-insert user)
        (accounts-update user (-> us first :id))))))
