(ns con-clj.core
  (:gen-class))

(defn sqsum
  [n]
  (reduce + (map #(* % %) (range n))))

(defn -main
  [& args]
  ; work around dangerous default behavior in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
