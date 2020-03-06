(ns rf-search.test.search
  (:require [clojure.test :refer :all]
            [rf-search.search :as search]))

(defonce ^:private index {"word1" '({:freq 4 :id "file1"} {:freq 1 :id "file2"})
                          "word2" '({:freq 10 :id "file1"} {:freq 3 :id "file3"})})

(deftest single-word-search
  (testing "searching a word that exists"
    (is (= '({:freq 4 :id "file1"} {:freq 1 :id "file2"})
           (search/single-word-search index "word1"))))

  (testing "searching a word that doesn't exist"
    (is (nil? (search/single-word-search index "word-1"))))
  )
