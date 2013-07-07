(ns con-clj.core
  (:gen-class))

(def squares
    (map #(* % %) (rest (range))))

(defn sq-sum
  [n]
  (reduce + (take n squares)))

(def fst first)
(def snd (comp first rest))

(defn fib [n]
  (let [f (fn)]
  (loop [i n a 0 b 1]
        (if (= 0 i)
            '()
            (recur ))

  (list 0 1 ())

(defn des [s]
  (let [[a b] s]
       (+ a b)))

(defn -main
  [& args]
  ; work around dangerous default behavior in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (sq-sum 4) "should be" (+ 1 4 9 16))
  (println (take 5 squares))
  (println (take 5 fib))
  (println (des [1 2 3]))
  )
