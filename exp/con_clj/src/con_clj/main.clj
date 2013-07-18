(ns con-clj.main
  (:gen-class)
  (:require [clojure.string :refer [join]]))

(def squares
    (map #(* % %) (rest (range))))

(defn sq-sum [n]
  (reduce + (take n squares)))

(def fst first)
(def snd (comp first rest))

(defn n-fib [n]
  (letfn [(gen [i a b]
             (if (zero? i)
                ()
                (cons b (gen (dec i) b (+ a b)))))]
    (gen n 0 1)))

(defn des [s]
  (let [[a b] s]
       (+ a b)))

(defn primes [n]
  (vec (range n)))

(defn -main
  [& args]
  ; work around dangerous default behavior in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (sq-sum 4) "should be" (+ 1 4 9 16))
  (println (take 5 squares))
  (println (des '(10 2 3)))
  (println (n-fib 7))
  (println (map (partial * 100) (range 10)))
  (println (join ", " [1 2 3 4]))
  )
