(ns markov-elear.generator
  (:require [clojure.string :as streng]
            [clojure.set :as mengde]
            [clojure.java.io :as io!]))

(def example "And the Golden Grouse And the Pobble who")
(def words (streng/split example #" "))
(def word-transitions (partition-all 3 1 words))
(def prefix-list ["On the" "They went" "And all" "We think"
                  "For every" "No other" "To a" "And every"
                  "We, too," "For his" "And the" "But the"
                  "Are the" "The Pobble" "For the" "When we"
                  "In the" "Yet we" "With only" "Are the"
                  "Though the"  "And when"
                  "We sit" "And this" "No other" "With a"
                  "And at" "What a" "Of the"
                  "O please" "So that" "And all" "When they"
                  "But before" "Whoso had" "And nobody" "And it's"
                  "For any" "For example," "Also in" "In contrast"])

(defn end-at-last-punctuation
  [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (streng/replace result-text #"[,| ]$" ".")]
    (streng/replace cleaned-text #"\"" "'")))

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

(defn process-file
  [fname]
  (text->markov-chain
   (slurp (io!/resource fname))))

(def files ["quangle-wangle.txt" "monad.txt"])
(def functional-leary (apply merge-with mengde/union (map process-file files)))

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

(defn tweet-text
  []
  (let [text (generate-text (-> prefix-list shuffle first) functional-leary)]
    (end-at-last-punctuation text)))
