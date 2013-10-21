(ns web.data
  (:require
    [korma.db]
    [korma.core :as dsl]
    [clojure.java.jdbc :as :jdbc]
    [clojure.java.jdbc.sql :as sql]
    [clojure.java.jdbc.ddl :as ddl]))

(korma.db/defdb db-spec
  (korma.db/h2 {:kdb "tcp://localhost:9092/~/local"
                :user "sa"
                :password ""}))

(dsl/defentity users)

(def save-user
  [user]
  nil)

; (def ^:const MALE "male")
; (def ^:const FEMALE "female")

; ^ means indexed.
; Account table
; ^id | ^email | gender | name | given_name | birthday | picture | locale

; (defentity accounts
;   (entity-fields :email :name :given_name :gender :birthday :picture :locale))

; (defn save-user
;   [user]
;   (with-open [^java.sql.Connection con (jdbc/get-connection db-spec)]
;     (jdbc/db-transaction [db (jdbc/add-connection db-spec con)] (fn [db]
;         (prn (sql/select * (sql/where {:email "igor.demura@gmail.com"})))))))
; ; (jdbc/insert! db :accounts user))))

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

      ; ;; Insert some value...
      ; (jdbc/insert! db :accounts
      ;   {:email "igor.demura@gmail.com"
      ;    :gender MALE
      ;    :name "Igor Demura"
      ;    :given_name "Igor"
      ;    :birthday "1985-08-09"
      ;    :locale "en"}))))

(defn play-data
  []
  (prn (dsl/select users)))
