(ns con-clj.test
  (:gen-class)
  (:require [clojure.string :refer [join]]
            [clojure.contrib.math :as math]))

(defn sq-sum [n]
  (reduce #(+ % (* %2 %2)) 0 (take n (iterate inc 1))))

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

; Gets a square root of n, if it isn't exact int, returns int truncated down.
(defn sqrt-int [n]
  (math/round (- (math/sqrt n) 0.5)))

(defn primes [n]
  (let [mark (fn [i di v]
               (if (<= i (count v))
                 (recur (+ i di) di (assoc v (dec i) di))
                 v))
        step (fn [i ps v]
               (if (<= i (count v))
                 (if (= (v (dec i)) 1)
                   (recur (inc i) (conj ps i) (mark (* i i) i v))
                   (recur (inc i) ps v))
                 ps))]
    (->> (repeat 1) (take n) vec (step 2 []))))

; (defn step-do [start step v]
;   (last (for [i (range start (count v) step)]
;           (assoc v i (* 10 (v i))))))

(defn step-do [start step v]
  (reduce (fn [v i] (assoc v i (* 10 (v i))))
    v
    (range start (count v) step)))

(defn clone-coll [c]
  (into (empty c) c))

(defn -main
  [& args]
  ; work around dangerous default behavior in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (sq-sum 4) "should be" (+ 1 4 9 16))
  (println (des '(10 2 3)))
  (println (n-fib 7))
  (println (map (partial * 5) (range 5)))
  (println (join ", " [1 2 3 4]))
  (println (sqrt-int 4) (sqrt-int 5))
  (println "Primes test:")
  (println (primes 19))
  (println (quot 3 2) (/ 3 2))
  (println (step-do 2 3 (vec (range 1 15))))
  (let [c [1 3 7 10]]
    (println "Clone of" c "is" (clone-coll c)))
)
