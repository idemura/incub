(ns web.datadef
  (:require
    [clojure.java.jdbc :as :jdbc]
    [clojure.java.jdbc.sql :as sql]
    [clojure.java.jdbc.ddl :as ddl])))

(def ^:private db-spec {
  :classname "org.h2.Driver"
  :subprotocol "h2"
  :subname "tcp://localhost:9092/~/local"
  :user "sa"
  :password ""}))

(defn init-db
  []
  (with-open [^java.sql.Connection con (jdbc/get-connection db-spec)]
    (jdbc/db-transaction [db (jdbc/add-connection db-spec con)]
      ;; Create a table with indices.
      (jdbc/db-do-commands db true
        (ddl/create-table :accounts
          [:id "INTEGER" "PRIMARY KEY" "AUTO_INCREMENT"]
          [:email "VARCHAR(95) UNIQUE"]
          [:gender "VARCHAR(15)"]
          [:name "VARCHAR(95)"]
          [:given_name "VARCHAR(95)"]
          [:birthday "DATE"]
          [:picture "VARCHAR(255)"]
          [:locale "VARCHAR(15)"])
        (ddl/create-index :by_email :accounts
          [:email] :unique)))))
