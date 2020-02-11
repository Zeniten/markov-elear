(ns markov-elear.generator
  (:require [clojure.string :as streng]
            [clojure.set :as mengde]))

(def example "And the Golden Grouse And the Pobble who")
(def words (streng/split example #" "))
(def word-transitions (partition-all 3 1 words))

(defn markov-chain
  [word-transitions]
  (reduce (fn [r t]
            (merge-with mengde/union r
                        (let [[a b c] t]
                          {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->markov-chain
  [s]
  (let [words (streng/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (markov-chain word-transitions)))
