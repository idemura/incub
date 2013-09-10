(ns tree.main
  (:gen-class)
  (:require [clojure.java.io]
            [clojure.string :refer [join]])
  (:import java.io.File))

(def ^:const H_LINE "\u2500")
(def ^:const V_LINE "\u2502")
(def ^:const BRANCH "\u251c")
(def ^:const LAST_BRANCH "\u2514")

(defn render-line [st ^File file]
  (if (empty? st)
    (.getName file)
    (str (join "  "
               (map #((if (= %2 (count st))
                        {:n BRANCH :e LAST_BRANCH}
                        {:n V_LINE :e " "}) %1)
                    st (rest (range))))
         H_LINE " " (.getName file))))

(defn walk [dirs ^File file st]
  (println (render-line st file))
  (if-let [child (sort-by (fn [^File f] (.getName f)) (dirs (.getPath file)))]
    (let [n (count child)
          pred (fn [new-file i]
                 (walk dirs new-file
                       (conj st (if (= i n) :e :n))))]
      (dorun (map pred child (rest (range)))))
    nil))

(defn filter-files [regex files]
  (if regex
    (let [re (re-pattern regex)
          pr (fn [^File f] (or (.isDirectory f) (re-find re (.getName f))))]
      (filter pr files))
    files))

(def ^:const HELP_STRING
  "Pretty print of a directory's file tree.
Usage:
    tree <directory> [regex file filter]
  ")

;; TODO: Use transients.
(defn -main [& [start-dir-name regex]]
  ;; Work around dangerous default behaviour in Clojure.
  (alter-var-root #'*read-eval* (constantly false))
  (if (nil? start-dir-name)
    (println HELP_STRING)
    (let [start (clojure.java.io/file start-dir-name)
          files (filter-files regex (file-seq start))
          assoc-file (fn [m ^File f]  ;; m is map String->File
                       (if-let [p (.getParent f)]
                         (assoc m p (conj (m p []) f))
                         m))
          dirs (reduce assoc-file {} files)]
      (walk dirs start []))))
