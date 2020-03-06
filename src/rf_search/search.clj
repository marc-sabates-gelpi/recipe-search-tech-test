(ns rf-search.search
  (:require [clojure.string :as string]))

(def ^:private max-entries 10)
(def ^:private debug? false)

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
  "Return a map with the current search word and the others as collocations."
  [words]
  (let [words (vec words)]
    (for [n (range (count words))]
      {:word         (get words n)
       :collocations (map #(get words %) (remove #{n} (range (count words))))})))

(defn get-files-by-collocation
  "Return a list of maps with the matches by collocation.
  The maps have two keys `:num` num of collocated words matching and `:file`."
  [all collocated-words]
  (reduce (fn [result {:keys [collocations file]}]
            (let [num (->> collocations
                           (filter collocated-words)
                           count)]
              (if (> num 1)
                (conj result {:num num :file file})
                result)))
          '()
          all))

(defn search-with-collocations
  "Return a map with the results by collocation and the rest of results for the given `word`.
  The results by collocation are calculated by checking that any of the collocated words to the search word,
  match with any of the collocations on the files indexed for the given word."
  [index {:keys [word collocations] :as _group}]
  (let [file-entries          (get index word)
        collocations          (set collocations)
        files-by-collocations (get-files-by-collocation file-entries collocations)
        by-collocations-ids   (map (comp :id :file) files-by-collocations)]
    {:with-collocations    files-by-collocations
     :without-collocations (if (seq files-by-collocations)
                             (remove (comp #{by-collocations-ids} :id) file-entries)
                             file-entries)}))

(defn multiple-word-search
  "Return a collection of ordered results taking into consideration collocations.
  Ordered desc by collocations num and then desc by frequency.
  It could be interesting to deem if high freqs can trump collocation."
  [index words]
  (let [word-groups      (make-groups words)
        results-by-group (map (partial search-with-collocations index) word-groups)]
    (concat (map :file (sort-by :num > (mapcat :with-collocations results-by-group)))
            (sort-by :freq > (mapcat :without-collocations results-by-group)))))

(defn word-search
  "Return the search depending on whether there is one word on many."
  [index words]
  (if (= 1 (count words))
    (single-word-search index (first words))
    (multiple-word-search index words)))

(defn get-details
  "Return the file details or only the id depending on the `debug?` flag."
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
