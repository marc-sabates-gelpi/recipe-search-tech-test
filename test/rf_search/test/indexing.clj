(ns rf-search.test.indexing
  (:require [clojure.test :refer :all]
            [rf-search.indexing :as indexing]))

(deftest get-adjacents
  (testing "adjacency happy path"
    (is (= '({:word "word1", :collocations #{"word2" "word3"}}
             {:word "word2", :collocations #{"word1" "word3" "word4"}}
             {:word "word3", :collocations #{"word1" "word2" "word4" "word5"}}
             {:word "word4", :collocations #{"word2" "word3" "word5"}}
             {:word "word5", :collocations #{"word3" "word4"}})
           (indexing/get-adjacents 2 ["word1" "word2" "word3" "word4" "word5"]))))

  (testing "adjacency radius 0"
    (is (= '({:word "word1", :collocations #{}}
             {:word "word2", :collocations #{}}
             {:word "word3", :collocations #{}}
             {:word "word4", :collocations #{}}
             {:word "word5", :collocations #{}})
           (indexing/get-adjacents 0 ["word1" "word2" "word3" "word4" "word5"]))))

  (testing "adjacency negative radius"
    (is (= '({:word "word1", :collocations #{}}
             {:word "word2", :collocations #{}}
             {:word "word3", :collocations #{}}
             {:word "word4", :collocations #{}}
             {:word "word5", :collocations #{}})
           (indexing/get-adjacents -2 ["word1" "word2" "word3" "word4" "word5"]))))

  (testing "adjacency empty coll"
    (is (= '()
           (indexing/get-adjacents 0 []))))

  (testing "adjacency nil coll"
    (is (= '()
           (indexing/get-adjacents 0 nil)))))

(deftest file->index
  (testing "happy path"
    (is (= {"these"    '({:id "path/file" :freq 1 :collocations #{"contents"}})
            "contents" '({:id "path/file" :freq 1 :collocations #{"these" "test"}})
            "test"     '({:id "path/file" :freq 1 :collocations #{"contents" "file"}})
            "file"     '({:id "path/file" :freq 1 :collocations #{"test" "with"}})
            "with"     '({:id "path/file" :freq 1 :collocations #{"file" "words"}})
            "words"    '({:id "path/file" :freq 2 :collocations #{"with" "longer" "three"}})
            "longer"   '({:id "path/file" :freq 1 :collocations #{"words" "than"}})
            "than"     '({:id "path/file" :freq 1 :collocations #{"longer" "three"}})
            "three"    '({:id "path/file" :freq 1 :collocations #{"than" "words"}})}
           (indexing/file->index {:id "path/file" :content "These are the contents of a test file with words longer than three words."}))))

  (testing "only words without numbers and potentially dashes considered"
    (is (= {"file-descriptor" '({:id "path/file" :freq 1 :collocations #{"with"}})
            "with"            '({:id "path/file" :freq 1 :collocations #{"file-descriptor" "pointers"}})
            "pointers"        '({:id "path/file" :freq 1 :collocations #{"with"}})}
           (indexing/file->index {:id "path/file" :content "A file-descriptor with 3 'pointers'"}))))

  (testing "empty content"
    (is (= {}
           (indexing/file->index {:id "path/file" :content ""}))))

  (testing "nil content"
    (is (= {}
           (indexing/file->index {:id "path/file" :content nil})))))
