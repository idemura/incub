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

(defn seq-to-str [s]
  (apply str s))

(defn parse-csv [csv]
  (let [not-comma #(not= % \,)
        drop-to-comma #(drop-while not-comma %)
        take-to-comma #(take-while not-comma %)]
    (map (comp trim seq-to-str take-to-comma)
         (take-while identity (iterate (comp next drop-to-comma) csv)))))

;; Better to have a macro.
(defn throw-ex [& s]
  (throw (new Exception (apply str s))))

(defn space? [^Character c]
  (Character/isWhitespace c))

(defn digit? [^Character c]
  (= (Character/getType c)
     (Character/DECIMAL_DIGIT_NUMBER)))

(defn to-int [ds]
  (reduce #(+ (* 10 %1) %2) (map #(- (int %) (int \0)) ds)))

(defn take-int [s]
  (when (digit? (first s))
    (let [[digits rs] (split-with digit? s)]
      [rs {:int (to-int digits)}])))

(defn take-sym [s]
  (case (first s)
    \+ [(rest s) {:plus +}]
    \- [(rest s) {:minus -}]
    \* [(rest s) {:star *}]
    \/ [(rest s) {:slash /}]
    \( [(rest s) {:lbrace true}]
    \) [(rest s) {:rbrace true}]
    nil))

(defn tokens [expr]
  (loop [ex expr, ts []]
    (let [nsp (drop-while space? ex)]
      (if (empty? nsp)
        (conj ts {:eof true})
        (let [[rs t] (or (take-int nsp)
                         (take-sym nsp)
                         (throw-ex "Invalid symbol " (first nsp)))]
            (recur rs (conj ts t)))))))

(declare parse-expr parse-mult parse-prim)

(defn parse-prim [ts]
  (let [[t & ts-tail] ts]
    (cond
      (:int t)
        [(fn [_] (:int t)) ts-tail]
      (:lbrace t)
        (let [[lfn [t & ts-tail]] (parse-expr ts-tail)]
          (if (:rbrace t)
            [lfn ts-tail]
            (throw-ex "Right brace expected")))
      :else
        ;; TODO: Better diagnostic for right brace.
        (throw-ex "Integer or brace expression expected"))))

(defn parse-mult [ts]
  (loop [[lfn l-tail] (parse-prim ts)]
    (if-let [op (let [t (first l-tail)] (or (:star t) (:slash t)))]
      (let [[rfn r-tail] (parse-prim (rest l-tail))]
        (recur [#(op (lfn %) (rfn %)) r-tail]))
      [lfn l-tail])))

(defn parse-expr [ts]
  (loop [[lfn l-tail] (parse-mult ts)]
    (if-let [op (let [t (first l-tail)] (or (:plus t) (:minus t)))]
      (let [[rfn r-tail] (parse-mult (rest l-tail))]
        (recur [#(op (lfn %) (rfn %)) r-tail]))
      [lfn l-tail])))

(defn parse [ts]
  (let [[res [{v :eof}]] (parse-expr (seq ts))]
    (if v
      res
      (throw-ex "Binary op or EOF expected"))))

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
  ; (println (interpose "|" (parse-csv "1, 2 , 3 ,, end")))
  ; (println (> 0 (compare \a \c)))

  (let [expr " 12 / 2 * (1 + 1) + 2 * (6 - 1) "]
    (try
      (let [f (parse (tokens (seq expr)))]
        (println (trim expr) ":=" (f {})))
     (catch Exception e
       (println "Exception:" (str e)))))
)
