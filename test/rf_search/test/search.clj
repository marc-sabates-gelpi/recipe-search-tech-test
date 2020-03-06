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
    (is (nil? (search/single-word-search index "word-1")))))

(deftest word-treatment
  (testing "collection of words treated and in the same order"
    (is (= '("word1" "word2" "word3")
           (search/word-treatment '("Word1" "woRD2" "WORD3")))))
  (testing "empty collection"
    (is (= '()
           (search/word-treatment '()))))
  (testing "nil collection"
    (is (= '()
           (search/word-treatment nil))))
  (testing "collections without order may return in no specific order."
    (let [words (map (partial str "word") (range 100))]
      (is (not= words
                (search/word-treatment (set words)))))))

(deftest make-groups
  (testing "happy case"
    (is (= '({:word "word1" :collocations ("word2" "word3")}
             {:word "word2" :collocations ("word1" "word3")}
             {:word "word3" :collocations ("word1" "word2")})
           (search/make-groups '("word1" "word2" "word3")))))

  (testing "not enough words"
    (is (= '({:word "word1" :collocations ()})
           (search/make-groups '("word1")))))

  (testing "empty collection"
    (is (= '()
           (search/make-groups '()))))

  (testing "nil collection"
    (is (= '()
           (search/make-groups nil)))))
