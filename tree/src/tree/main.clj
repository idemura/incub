(ns tree.main
  (:gen-class)
  (:require
    [clojure.java.io])
  (:import java.io.File))

(def ^:const V_LINE "\u2502")
(def ^:const BRANCH "\u251c")
(def ^:const LAST_BRANCH "\u2514")

(defn render-line [st ^File file]
  (if (empty? st)
    (.getName file)
    (str (reduce #(str %1 "  " %2)
           (map #((if (= %2 (count st))
                    {:n BRANCH :e LAST_BRANCH}
                    {:n V_LINE :e " "}) %1)
             st (iterate inc 1)))
         "\u2500 " (.getName file))))

(defn walk [dirs ^File file st]
  (println (render-line st file))
  (if-let [child (dirs (.getPath file))]
    (let [n (count child)
          pred (fn [new-file i]
                 (walk dirs new-file
                       (conj st (if (= i n) :e :n))))]
      (dorun (map pred child (iterate inc 1)))
    nil)))

;; TODO: Use transients.
(defn -main
  [& args]
  ;; Work around dangerous default behaviour in Clojure.
  (alter-var-root #'*read-eval* (constantly false))
  (let [start-path (clojure.java.io/file (if (empty? args) "." (first args)))
        files (doall (file-seq start-path))
        assoc-file (fn [m ^File f]  ;; m is map, f is java.io.File
                     (let [p (.getParent f)]
                       (if p
                         (assoc m p (conj (m p []) f))
                         m)))
        dirs (reduce assoc-file {} files)]
    (walk dirs start-path [])))
