(ns rf-search.core
  (:require [clojure.java.io :refer [file as-file]]
            [clojure.string :as string]
            [digest :as digest]
            [clj-memory-meter.core :as mm]))

(def ^:private file-register (atom {}))
(def ^:private index (atom {}))

(defn file->index
  [{:keys [id content]}]
  (->> content
       (re-seq #"[a-zA-Z]{4,}")
       (map string/lower-case)
       frequencies
       (map (fn [[k v]] {k (list {:freq v :id id})}))
       (into {})))

(defn make-index
  [s]
  (let [files (->> s
                   file
                   file-seq
                   (map str)
                   rest                                     ;; first element is the dir
                   )
        ]
    (reset! index (->> files
                       (pmap (comp file->index #(array-map :id % :content (slurp %))))
                       (reduce (partial merge-with into) {})
                       (map (fn [[k v]] [k (reverse (sort-by :freq v))]))
                       (take 10)
                       (into {})))))

(defn -main
  []
  (println "Starting indexing")
  (make-index "resources/recipes"))

(comment (time (->> "resources/recipes"
                    file
                    file-seq
                    (map str)
                    rest                                                  ;; first element is the dir
                    (map #(vector % (digest/md5 (as-file %))))
                    (into {}))))
