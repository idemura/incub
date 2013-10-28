(ns web.data
  (:require
    [korma.config]
    [korma.db]
    [korma.core :as ql]))

(korma.db/defdb db-spec
  (korma.db/h2 {:db "tcp://localhost:9092/~/local"
                :user "sa"
                :password ""}))

(ql/defentity users
  (ql/table :accounts))

(defn configure-db
  []
  (korma.config/set-delimiters ""))

;; IN users: {} of columns to insert.
;; OUT: nil
(defn save-user
  [user]
  (korma.db/transaction
    ; (prn (ql/select (ql/fields :email) (ql/where {:email (:email user)})))
    (prn (:email user))
    (prn (ql/select (ql/where {:email (:email user)})))
    ; (ql/insert users (ql/values user)))
  ))

