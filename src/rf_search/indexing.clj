(ns rf-search.indexing
  (:require [clojure.string :as string]
            [clojure.java.io :refer [file as-file]]))

(def ^:private words-per-side 1)

(defn get-collocations
  "Return a collection of maps with each elem in `coll` and it's surrounding elems.
  It doesn't return elements outside of the range of `coll`.
  `radius` is the number of additional elems per side.
  Return example: `'({:word \"apple\" :collocations #{\"gala\" \"red\"}})`"
  ([radius coll]
   (map (partial get-collocations radius coll) (range (count coll))))
  ([radius coll center]
   (let [coll (vec coll)
         max  (dec (count coll))]
     {:word         (get coll center)
      :collocations (set
                      (for [ix (range (- center radius) (+ center radius 1))
                            :when (and (<= 0 ix max)
                                       (not= center ix))]
                        (get coll ix)))})))

(defn file->index
  "Return a map consisting of words as keys and
  their frequency within a file.
  See `get-index` for return format."
  [{:keys [id content] :as _file-details}]
  (let [words        (->> content
                          (re-seq #"[a-zA-Z\-]{4,}")        ;; Taking only words length >= 4
                          (map string/lower-case))          ;; Word treatment, e.g. noun filtering, plurals
        collocations (->> words
                          (get-collocations words-per-side)
                          (map (fn [{:keys [word collocations]}] {word collocations}))
                          (apply merge-with into))]
    (->> words
         frequencies
         (map (juxt key (fn [[w freq]]
                          (list {:freq         freq
                                 :id           id
                                 :collocations (get collocations w)}))))
         (into {}))))

(defn dir->files
  "Return the files in a directory.
  The return format is a collection of maps `{:id <file path>}.`"
  [s]
  (->> s
       file
       file-seq
       (map str)
       rest                                                 ;; first element is the dir
       (map (partial hash-map :id))))

(defn get-index
  "Return an index for the words in the coll of files `s`.
  The index is a map indexed by the word and the values a list of
  maps with the frequency of the word in that file, e.g.
  `{\"apple\" '({:freq 4 id: \"file path\"} {:freq 1 id: \"file path 2\"})
  \"pear\" '({:freq 7 id: \"file path\"} {:freq 2 id: \"file path 3\"})}`.
  The list in the values is sorted by `:freq`."
  [s]
  ;; FIXME: Indexing took 1.2 secs before the collocations
  ;;        after it takes 13 secs! :(
  (time (->> (dir->files s)
             (pmap (comp file->index #(assoc % :content (slurp (:id %)))))
             (reduce (partial merge-with into) {})
             (map (juxt key (comp (partial sort-by :freq >) val)))
             (into {}))))

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
