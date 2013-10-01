(ns web.data
  (:use web.generic korma.db))

(defdb mydb
  (h2 {:db "jdbc:h2:tcp://localhost/~/local"
       :user "sa"
       :password ""}))
