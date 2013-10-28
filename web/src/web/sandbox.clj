(ns web.sandbox
  (:gen-class)
  (:use web.data web.generic))

;; For easier REPL access.
(defn clj-main
  [& args]
  (configure-db)
  (save-user 
    {:email "neil182@mail.ru"
     :gender "male"
     :name "Leo Novikov"
     :given_name "Leo"
     :birthday "1985-05-01"
     :locale "ru"}))

(defn -main 
  [& args]
  (clj-main args))
