(ns rf-search.search
  (:require [clojure.string :as string]
            [taoensso.timbre :refer [spy debug]]))

(def ^:private max-entries 10)
(def ^:private debug? true)

(defn word-treatment
  "Apply the desired word treatments, e.g. spelling, plural vs singular.
  Currently only applies lower case."
  [words]
  (map string/lower-case words))

(defn single-word-search
  "Return the `word` entry in the index."
  [index word]
  (get index word))

(defn make-groups
  [words]
  (let [words (vec words)]
    (for [n (range (count words))]
      {:word         (get words n)
       :collocations (map #(get words %) (remove #{n} (range (count words))))})))

(defn search-with-collocations
  [index {:keys [word collocations] :as _group}]
  (let [file-entries          (get index word)
        collocations          (set collocations)
        files-by-collocations (spy (vec (remove (comp (partial >= 1) :num) (map #(array-map :num %1 :file %2) (map (comp count (partial filter collocations) :collocations) file-entries) file-entries))))
        by-collocations-ids   (map (comp :id :file) files-by-collocations)]
    {:with-collocations    files-by-collocations
     :without-collocations (remove (comp #{by-collocations-ids} :id) file-entries)}))

(defn multiple-word-search
  "Return a collection of ordered results taking into consideration collocations.
  Ordered desc by collocations num and then desc by frequency.
  It could be interesting to deem if high freqs can trump collocation."
  [index words]
  (let [word-groups      (make-groups words)
        results-by-group (map (partial search-with-collocations index) word-groups)]
    (concat (map :file (reverse (sort-by :num (mapcat :with-collocations results-by-group))))
            (reverse (sort-by :freq (mapcat :without-collocations results-by-group))))))

(defn word-search
  "Return the search depending on wether there is one word on many."
  [index words]
  (if (= 1 (count words))
    (single-word-search index (first words))
    (multiple-word-search index words)))

(defn get-details
  [entries]
  (if debug?
    entries
    (map :id entries)))

(defn search
  "Return the serach results or a no results message."
  [index words]
  (time (or (some->> words
                     word-treatment
                     (word-search index)
                     (take max-entries)
                     get-details)
            [">>> No results <<<"])))
