(ns rf-search.test-runner
  (:require [clojure.test :as t :refer :all]
            [rf-search.test.indexing]
            [rf-search.test.search]))

(defn -main []
  (t/run-tests 'rf-search.test.indexing)
  (t/run-tests 'rf-search.test.search))
