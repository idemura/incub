(ns con-clj.test
  (:gen-class)
  (:require
    [clojure.string :refer [join trim triml trimr]]
    [clojure.contrib.math :as math]))

(defn sq-sum [n]
  (reduce #(+ % (* %2 %2)) 0 (take n (iterate inc 1))))

(def fst first)
(def snd (comp first rest))

(def fib
  (letfn [(gen [a b]
            (cons b (lazy-seq (gen b (+ a b)))))]
    (gen 0 1)))

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

(defn bottles []
  (doseq [b (reverse (range 1 10))]
    (println b "bottles, take one, drink,"))
  (println "No bottles any more"))

(defn logf [f]
  (fn [& args]
    (let [r (apply f args)]
      (println args "->" r)
      r)))

(def abs-m (comp math/abs -))

; Can wrap logging in function and/or macro.
(defn diag [x1 y1 x2 y2]
  (= (abs-m x2 x1) (abs-m y2 y1)))

(defn queens [c vac st]
  (if (empty? vac)
    (if (= c 9) [st] [])
    (letfn [(free? [[x y] st]
              (not (some (fn [[a b]] (diag x y a b)) st)))]
      (mapcat
        #(queens (inc c) (disj vac %) (conj st [c %]))
        (set (filter #(free? [c %] st) vac))))))

(defn space? [c]
  (Character/isWhitespace c))

(defn tokens [expr]
  (loop [ex expr as []]
    (let [nsp (drop-while space? ex)
          fc (first nsp)
          rs (rest nsp)]
      (cond
        (empty? nsp) (conj as {:tok :eof})
        ; (<= \0 (first nsp) \9) (conj (tokens (rest expr)) {:tok :digit})
        :else (recur rs (conj as {:tok :sym :val fc}))))))

(defn -main [& args]
  ; Work around dangerous default behavior in Clojure.
  (alter-var-root #'*read-eval* (constantly false))
  ; (let [n 4]
  ;   (println "Sum of 1 .." n "squares is" (sq-sum 4) "check:" (+ 1 4 9 16)))
  ; (println (des '(10 2 3)))
  ; (println "Fibonacci:" (join " " (take 7 fib)) "...")
  ; (println (map (partial * 5) (range 5)))
  ; (println (join ", " [1 2 3 4]))
  ; (println "Trunc down sqrt 4:" (sqrt-int 4) "sqrt 5:" (sqrt-int 5))
  ; (println "Primes:" (primes 19))
  ; (println (quot 3 2) (/ 3 2))
  ; (println "Step iterate from:" (step-do 2 3 (vec (range 1 15))))
  ; (let [c [1 3 7 10]]
  ;   (println "Clone of" c "is" (clone-coll c)))
  ; (bottles)
  ; (let [sol (queens 1 (set (range 1 9)) ())]
  ;   (println sol)
  ;   (println (count sol) "solutions total."))
  (println (tokens (seq " 2 + 3")))
)