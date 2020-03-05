(ns rf-search.core
  (:require [clj-memory-meter.core :as mm]
            [clojure.edn :as edn]
            [clojure.java.io :refer [file as-file]]
            [clojure.string :as string]
            [juxt.dirwatch :refer [watch-dir close-watcher]]
            [taoensso.timbre :refer [debug spy]]))

(defonce ^:private index (atom {}))
(def ^:private max-entries 10)
(def ^:private recipes-dir "resources/recipes")

(defn file->index
  "Return a map consisting of words as keys and
  their frequency within a file.
  See `get-index` for return format."
  [{:keys [id content] :as _file-details}]
  (->> content
       (re-seq #"[a-zA-Z\-]{4,}")
       (map string/lower-case)
       frequencies
       (map (juxt key (comp #(list {:freq % :id id}) val)))
       (into {})))

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
  (->> (dir->files s)
       (pmap (comp file->index #(assoc % :content (slurp (:id %)))))
       (reduce (partial merge-with into) {})
       (map (juxt key (comp reverse (partial sort-by :freq) val)))
       (into {})))

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

(defn partial-index!
  "Update the `index` with the new file changes."
  [args]
  (println "Updating index..")
  (debug "---> updated files:" (some->> args
                                        (filter (comp #{:modify :delete} :action))
                                        (map (juxt (comp str :file) :action))
                                        (into {})))
  ;; TODO: Legacy process. Please implement partial update
  (reset! index (get-index recipes-dir))
  #_(let [{new-index :index new-register :register} (get-index "resources/recipes")]
      (swap! register merge new-register)
      (swap! index update-index new-index (keys register)))
  (println "Updating done.."))

(defn full-index!
  "Replace the index with a new index."
  []
  (println "Indexing..")
  (reset! index (get-index recipes-dir))
  (println "Indexing done.."))

(defn index!
  "Update or replace the `index` based on if there are
  individual files changed or not."
  [& args]
  (if (seq args)
    (partial-index! args)
    (full-index!)))

(defn get-option!
  "Prompt for the user option."
  []
  (println "Options:\ne or q\t-> exit\ns words\t-> search")
  (print "$ ")
  (flush)
  (let [input (string/split (read-line) #"\s")]
    (cond-> {:option (first input)}
            (seq (rest input)) (assoc :args (rest input)))))

(defn search
  "Return the serach results or a no results message."
  [words]
  (or (some->> words
               (map string/lower-case)
               first
               (get @index)
               (map :id)
               (take max-entries))
      [">>> No results <<<"]))

(defn search!
  "Print out the serach results for the given `words`."
  [words]
  (if (seq words)
    (printf "Search results:\n\n%s\n\n" (apply str (interpose "\n" (search words))))
    (println ">>> No search terms introduced <<<")))

(def exit? #{"e" "q"})
(def search? #{"s"})
(def index? #{"i"})

(defn -main
  "Start the program and run indexing for the first time."
  []
  (let [w (watch-dir index! (file recipes-dir))]
    (println "Starting..")
    (index!)
    (loop [option nil args nil]
      (when-not (exit? option)
        (cond
          (search? option) (search! args)
          (index? option) (index!))
        (let [input (get-option!)]
          (recur (:option input) (:args input)))))
    (println "Exiting!")
    (close-watcher w)
    nil))


(comment (mm/measure (-main)))
