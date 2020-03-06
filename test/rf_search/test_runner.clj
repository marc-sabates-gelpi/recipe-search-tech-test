(ns rf-search.test-runner
  (:require [clojure.test :as t :refer :all]
            [rf-search.test.indexing]))

(defn -main []
  (t/run-tests 'rf-search.test.indexing))
