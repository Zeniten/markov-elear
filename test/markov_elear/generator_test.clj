(ns markov-elear.generator-test
  (:require [clojure.test :refer :all]
            [markov-elear.generator :refer :all]))

(deftest test-word-chain
  (testing "It produces a chain of the possible two step transitions between suffixes and prefixes:"
    (let [example '(("And" "the" "Golden")
                    ("the" "Golden" "Grouse")
                    ("And" "the" "Pobble")
                    ("the" "Pobble" "who"))]
      (is (= {["the" "Pobble"] #{"who"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (markov-chain example))))))
