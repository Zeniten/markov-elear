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

(defn chain->text
  [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn generate-text
  [start-phrase markov-chain]
  (let [prefix (streng/split start-phrase #" ")
        result-chain (walk-chain prefix markov-chain prefix)
        result-text (chain->text result-chain)]
    result-text))
