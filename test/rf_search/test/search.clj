(ns rf-search.test.search
  (:require [clojure.test :refer :all]
            [rf-search.search :as search]))

(def ^:private index {"word1" '({:freq 4 :id "file1" :collocations #{"word3"}} {:freq 1 :id "file2"})
                      "word2" '({:freq 10 :id "file1"} {:freq 3 :id "file3" :collocations #{"word4"}})
                      "word3" '({:freq 1 :id "file1"})
                      "word4" '({:freq 1 :id "file3" :collocations #{"word1"}})})

(deftest single-word-search
  (testing "searching a word that exists"
    (is (= '({:freq 4 :id "file1" :collocations #{"word3"}} {:freq 1 :id "file2"})
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
    (is (= '({:word "word1" :collocations #{"word2" "word3"}}
             {:word "word2" :collocations #{"word1" "word3"}}
             {:word "word3" :collocations #{"word1" "word2"}})
           (search/make-groups '("word1" "word2" "word3")))))

  (testing "not enough words"
    (is (= '({:word "word1" :collocations #{}})
           (search/make-groups '("word1")))))

  (testing "empty collection"
    (is (= '()
           (search/make-groups '()))))

  (testing "nil collection"
    (is (= '()
           (search/make-groups nil)))))

(deftest distinct-results
  (testing "happy case"
    (is (= [{:id "file1" :details :a}
            {:id "file2" :details :b}
            {:id "file3" :details :d}]
           (search/distinct-results '({:id "file1" :details :a}
                                      {:id "file2" :details :b}
                                      {:id "file1" :details :c}
                                      {:id "file3" :details :d})))))

  (testing "no repeated elems"
    (is (= [{:id "file1" :details :a}
            {:id "file2" :details :b}
            {:id "file3" :details :c}]
           (search/distinct-results '({:id "file1" :details :a}
                                      {:id "file2" :details :b}
                                      {:id "file3" :details :c})))))

  (testing "empty collection"
    (is (= []
           (search/distinct-results '()))))

  (testing "nil collection"
    (is (= []
           (search/distinct-results nil)))))

(deftest get-files-by-collocation
  (testing "found 2 files, one with 2 collocations found"
    (is (= '({:num 2 :file {:id "file3" :collocations #{"e" "f"}}}
             {:num 1 :file {:id "file1" :collocations #{"a" "b"}}})
           (search/get-files-by-collocation
             '({:id "file1" :collocations #{"a" "b"}}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             #{"a" "e" "f"}))))

  (testing "no files found"
    (is (= '()
           (search/get-files-by-collocation
             '({:id "file1" :collocations #{"a" "b"}}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             #{"1" "2" "3"}))))

  (testing "empty collocations file"
    (is (= '()
           (search/get-files-by-collocation
             '({:id "file1" :collocations #{}}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             #{"1" "2" "3"}))))

  (testing "nil collocations file"
    (is (= '()
           (search/get-files-by-collocation
             '({:id "file1" :collocations nil}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             #{"1" "2" "3"}))))

  (testing "empty available files list"
    (is (= '()
           (search/get-files-by-collocation
             '()
             #{"1" "2" "3"}))))

  (testing "nil available files list"
    (is (= '()
           (search/get-files-by-collocation
             nil
             #{"1" "2" "3"}))))

  (testing "empty collocated words to search"
    (is (= '()
           (search/get-files-by-collocation
             '({:id "file1" :collocations #{"a" "b"}}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             #{}))))

  (testing "nil collocated words to search"
    (is (= '()
           (search/get-files-by-collocation
             '({:id "file1" :collocations #{"a" "b"}}
               {:id "file2" :collocations #{"c" "d"}}
               {:id "file3" :collocations #{"e" "f"}}
               {:id "file4" :collocations #{"g" "h"}})
             nil)))))

(deftest search-with-collocations
  (testing "(swc1) search word `word2` collocations `#{\"word4\"}`"
    (is (= {:by-collocation '({:num 1 :file {:freq 3 :id "file3" :collocations #{"word4"}}})
            :by-word        '({:freq 10 :id "file1"})}
          (search/search-with-collocations index {:word "word2" :collocations #{"word4"}}))))

  (testing "(swc2) search word `word4` collocations `#{\"word2\"}`"
    (is (= {:by-collocation '()
            :by-word        '({:freq 1 :id "file3" :collocations #{"word1"}})}
           (search/search-with-collocations index {:word "word4" :collocations #{"word2"}})))))

(deftest multiple-word-search
  ;; This search should be the result of merging the results for (swc1) and (swc2). See above
  (testing "searching for `word2` `word4`"
    (is (= '({:freq 3 :id "file3" :collocations #{"word4"}} ; => found by word `word2` and collocation `word4`
             {:freq 10 :id "file1"}                         ; => found by word `word2`
             {:freq 1 :id "file3" :collocations #{"word1"}}) ; => found by word `word4`
           (search/multiple-word-search index '("word2" "word4"))))))
