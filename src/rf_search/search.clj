(ns rf-search.search
  (:require [clojure.string :as string]))

(def ^:private max-entries 10)

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
  "Return a collection of maps with the current search word and the others as collocations.
  It does it for every word in the collection."
  [words]
  (let [words (vec words)]
    (for [n (range (count words))]
      {:word         (get words n)
       :collocations (set (map #(get words %) (remove #{n} (range (count words)))))})))

(defn get-files-by-collocation
  "Return a list of maps with the matches by collocation.
  The maps have two keys `:num` num of collocated words matching and `:file`."
  [all collocated-words]
  (reduce (fn [result {:keys [collocations] :as file}]
            (let [num (->> collocations
                           (filter collocated-words)
                           count)]
              (if (pos? num)
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
        files-by-collocations (get-files-by-collocation file-entries collocations)
        by-collocations-ids   (map (comp :id :file) files-by-collocations)]
    {:by-collocation files-by-collocations
     :by-word        (if (seq by-collocations-ids)
                       (remove (comp #{by-collocations-ids} :id) file-entries)
                       file-entries)}))

(defn distinct-results
  "Return a collection in the same order given without repeated file ids.
  If a file id occurs in more than one elem, the details from the former
  (left-to-right) will be the prevailing in the result."
  [results]
  (reduce (fn [unified {:keys [id] :as elem}]
            (if (some (comp #{id} :id) unified)
              unified
              (conj unified elem)))
          []
          results))

(defn unify-results
  "Return an ordered collection of search results.
  The input is a collection of maps with keys `:by-collocations` and `:by-word`.
  The order is: desc by num of collocations and desc by frequency of word."
  [results]
  (concat (map :file (sort-by :num > (mapcat :by-collocation results)))
          (sort-by :freq > (mapcat :by-word results))))

(defn multiple-word-search
  "Return a collection of ordered results taking into consideration collocations.
  Ordered desc by collocations num and then desc by frequency.
  It could be interesting to deem if high freqs can trump collocation."
  [index words]
  (->> words
       make-groups
       (map (partial search-with-collocations index))
       unify-results))

(defn word-search
  "Return the search depending on whether there is one word on many."
  [index words]
  (if (= 1 (count words))
    (single-word-search index (first words))
    (multiple-word-search index words)))

(defn search
  "Return the search results or a no results message."
  [index words]
  (time (or (some->> words
                     word-treatment
                     (word-search index)
                     distinct-results
                     (take max-entries)
                     (map :id))
            [">>> No results <<<"])))
