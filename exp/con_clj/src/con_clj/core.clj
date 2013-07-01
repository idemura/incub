(ns con-clj.core
  (:gen-class))

(def squares (map #(* % %) (rest (range))))

(defn sq-sum
  [n]
  (reduce + (take n squares)))

(defn -main
  [& args]
  ; work around dangerous default behavior in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (sq-sum 4) "should be" (+ 1 4 9 16))
  (println (take 5 squares)))
