(ns rf-search.test.indexing
  (:require [clojure.test :refer :all]
            [rf-search.indexing :as indexing]))

(deftest get-collocations
  (testing "collocation happy path"
    (is (= '({:word "word1", :collocations ("word2" "word3")}
             {:word "word2", :collocations ("word1" "word3" "word4")}
             {:word "word3", :collocations ("word1" "word2" "word4" "word5")}
             {:word "word4", :collocations ("word2" "word3" "word5")}
             {:word "word5", :collocations ("word3" "word4")})
           (indexing/get-collocations 2 '("word1" "word2" "word3" "word4" "word5")))))

  (testing "collocation 0 radius"
    (is (= '({:word "word1", :collocations ()}
             {:word "word2", :collocations ()}
             {:word "word3", :collocations ()}
             {:word "word4", :collocations ()}
             {:word "word5", :collocations ()})
           (indexing/get-collocations 0 '("word1" "word2" "word3" "word4" "word5")))))

  (testing "collocation negative radius"
    (is (= '({:word "word1", :collocations ()}
             {:word "word2", :collocations ()}
             {:word "word3", :collocations ()}
             {:word "word4", :collocations ()}
             {:word "word5", :collocations ()})
           (indexing/get-collocations -2 '("word1" "word2" "word3" "word4" "word5")))))

  (testing "collocation empty coll"
    (is (= '()
           (indexing/get-collocations 0 '()))))

  (testing "collocation nil coll"
    (is (= '()
           (indexing/get-collocations 0 nil)))))
