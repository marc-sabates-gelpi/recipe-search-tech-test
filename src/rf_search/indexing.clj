(ns rf-search.indexing
  (:require [clojure.string :as string]
            [clojure.java.io :refer [file as-file]]))

(def ^:private words-per-side 1)

(defn get-adjacents
  "Return a collection of maps with each elem in vector `v` and it's surrounding elems.
  It doesn't return elements outside of the range of `v`.
  `radius` is the number of additional elems per side.
  Return example: `'({:word \"apple\" :collocations #{\"gala\" \"red\"}})`"
  ([radius v]
   (map (partial get-adjacents radius v) (range (count v))))
  ([radius v centre]
   (let [max (dec (count v))]
     {:word         (get v centre)
      :collocations (set
                      (for [ix (range (- centre radius) (+ centre radius 1))
                            :when (and (<= 0 ix max)
                                       (not= centre ix))]
                        (get v ix)))})))

(defn file->index
  "Return a map consisting of words as keys and
  their frequency within a file.
  See `get-index` for return format."
  [{:keys [id content] :as _file-details}]
  (if (seq content)
    (let [words        (->> content
                            (re-seq #"[a-zA-Z\-]{4,}")      ;; Taking only words length >= 4
                            (mapv string/lower-case))       ;; Word treatment, e.g. noun filtering, plurals
          collocations (->> words
                            (get-adjacents words-per-side)
                            (map (fn [{:keys [word collocations]}] {word collocations}))
                            (apply merge-with into))]
      (->> words
           frequencies
           (map (juxt key (fn [[w freq]]
                            (list {:freq         freq
                                   :id           id
                                   :collocations (get collocations w)}))))
           (into {})))
    {}))

(defn dir->files
  "Return the files in a directory.
  The return format is a collection of maps `{:id <file path>}.`"
  [dir-path]
  (->> dir-path
       file
       file-seq
       (map str)
       rest                                                 ;; first element is the dir
       (map (partial hash-map :id))))

(defn get-index
  "Return an index for the words in the coll of files inside `dir-path`.
  The index is a map indexed by the word and the values a list of
  maps with the frequency of the word in that file, and collocated words, e.g.
  `{\"apple\" '({:freq 4 id: \"file path\" :collocations #{\"gala\"}}
  {:freq 1 id: \"file path 2\" :collocations #{\"cider\"}})
  \"pear\" '({:freq 7 id: \"file path\"} {:freq 2 id: \"file path 3\"})}`."
  [dir-path]
  (time (->> (dir->files dir-path)
             (pmap (comp file->index #(assoc % :content (slurp (:id %)))))
             (reduce (partial merge-with into) {}))))

#_(defn merge-freqs
    "Return a merged list with `new` replacing `old` by `:id`."
    [old new]
    (let [new-ids (map :id new)
          _ (debug "---> new" new)
          _ (debug "---> old" old)
          _ (debug "---> new-ids" new-ids)]
      (spy (into (remove #(some (-> % :id hash-set) new-ids) old) new))))

#_(defn update-index
    "Return `old` replacing the word frequency on a file for the one in `new`.
    TODO: Remove words and remove files within words."
    [old new files]
    (merge-with merge-freqs old new))

;;;; Partial update steps:
; 0) NOTE to consider: Would this functionality be so much more efficient?
; 1) Add a register indexing file -> words (will be used on partial and full indexing)
; 2) On partial update remove the updated/removed file from the register
; 3) On partial update remove the updated/removed file from index for each word in the register
; 4) On partial update run "full update" on the updated files.
