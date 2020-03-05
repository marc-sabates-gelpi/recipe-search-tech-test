(ns rf-search.search
  (:require [clojure.string :as string]))

(def ^:private max-entries 10)

(defn word-treatment
  "Apply the desired word treatments, e.g. spelling, plural vs singular.
  Currently only applies lower case."
  [words]
  (map string/lower-case words))

(defn search
  "Return the serach results or a no results message."
  [index words]
  (time (or (some->> words
                     word-treatment
                     first
                     (get index)
                     (map :id)
                     (take max-entries))
            [">>> No results <<<"])))
