(ns rf-search.core
  (:require [clj-memory-meter.core :as mm]
            [clojure.java.io :refer [file as-file]]
            [clojure.string :as string]
            [digest :as digest]
            [taoensso.timbre :refer [debug spy]]))

(defonce ^:private register (atom {}))
(defonce ^:private index (atom {}))

(defn file->index
  "Return a map consisting of words as keys and
  their frequency within a file."
  [{:keys [id content] :as _file-details}]
  (->> content
       (re-seq #"[a-zA-Z]{4,}")
       (map string/lower-case)
       frequencies
       (map (juxt key (comp #(list {:freq % :id id}) val)))
       (into {})))

(defn file->md5
  "Return the md5sum of a given file `id` when it differs
  from the registered one."
  [id]
  (let [md5 (digest/md5 (as-file id))]
    (when-not (= (get @register id) md5)
      {:id id :md5 md5})))

(defn dir->files
  "Return the files in a directory and their md5sum.
  It runs the list against `register` and filters out
  unchanged files."
  [s]
  (->> s
       file
       file-seq
       (map str)
       rest                                                 ;; first element is the dir
       (take 10)
       (keep file->md5)))

(defn get-index-register
  [s]
  (let [files    (dir->files s)
        index    (->> files
                      (pmap (comp file->index #(assoc % :content (slurp (:id %)))))
                      (reduce (partial merge-with into) {})
                      (map (juxt key (comp reverse (partial sort-by :freq) val)))
                      (into {}))
        register (into {} (map (juxt :id :md5) files))]
    {:index index :register register}))

(defn -main
  []
  (println "Starting indexing")
  (get-index-register "resources/recipes"))


(comment (mm/measure (-main)))
