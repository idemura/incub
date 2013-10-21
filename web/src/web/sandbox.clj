(ns web.test
  (:gen-class)
  (:use web.data web.generic web.view))

(defn main
  [& args]
  ; (init-db)
  (save-user 
    {:email "igor.demura@gmail.com"
     :gender MALE
     :name "Igor Demura"
     :given_name "Igor"
     :birthday "1985-08-09"
     :locale "en"}))

(defn -entry_main 
  [& args]
  (main args))
