(ns rf-search.core
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as string]
            [juxt.dirwatch :refer [watch-dir close-watcher]]
            [rf-search.indexing :as indexing]
            [rf-search.search :as search]
            [taoensso.timbre :refer [debug spy]]))

(defonce ^:private index (atom {}))
(def ^:private recipes-dir "resources/recipes")

(defn partial-index!
  "Update the `index` with the new file changes."
  [args]
  (let [updates (->> args
                     (filter (comp #{:modify :delete} :action))
                     (map (juxt (comp str :file) :action))
                     (into {}))]
    (when (seq updates)
      (println "Updating index..")
      ;; TODO: Implement partial update
      (reset! index (indexing/get-index recipes-dir))
      (println "Updating done.."))))

(defn full-index!
  "Replace the index with a new index."
  []
  (println "Indexing..")
  (reset! index (indexing/get-index recipes-dir))
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

(defn search!
  "Print out the serach results for the given `words`."
  [words]
  (if (seq words)
    (printf "Search results:\n\n%s\n\n" (apply str (interpose "\n" (search/search @index words))))
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
