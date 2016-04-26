(ns demo.core
  (:require [clojure.pprint :as pprint])
  (:require [clojure.string :as str])
  (:gen-class))

(def vowels
	"aeiou")

(defn contains-char? [the-string the-char]
         (some #(= the-char %) the-string))

(defn- extract-words-from [words] 
  (let
    [remove-punctuation 
     (comp (partial apply str)
           (partial filter 
                    #(or (Character/isLetter %) 
                         (Character/isSpace %))))]
    (str/split (remove-punctuation (str/lower-case words)) #"\s+")))

(defn read-file [filename]
	(extract-words-from (slurp filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rough algorithm for calculating syllable count
;; counts the transitions from vowel to consonant
(defn pair-up [word]
	(partition 2 1 word))

(defn syll-matrix [pairs]
	(map (fn [pair] 
			(let [char1 (nth pair 0) char2 (nth pair 1)]
				(if (and (contains-char? vowels char1)
						 (not  (contains-char? vowels char2)))
				1
				0))) 
		pairs))

(defn syll-count [word]
	(let [count (reduce + (syll-matrix (pair-up word)))] 
		(if (not (= count 0))
			count
			1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sum-syllables [word-map]
	(reduce + (map :syllables word-map)))

(defn haikuify [word-syl-map]
	(loop [words word-syl-map 
		   acc []]
		(if (= (count words) 0)
			acc
			(let [haiku (reduce (fn [so-far curr]
							(let [sum-so-far (sum-syllables so-far) 
								  new-sum    (sum-syllables (conj so-far curr))]
								(cond
									(= sum-so-far 17) (reduced so-far)
									(> new-sum 17) (reduced [])
									(and (< sum-so-far 12) (> new-sum 12)) (reduced [])
									(and (< sum-so-far 5)  (> new-sum 5))  (reduced [])
									:else (conj so-far curr))))
						[{:word "" :syllables 0}] 
						words)]
		(recur (rest words) (conj acc haiku))))))

;; first removes each dummy word from the pair
;; removes empty haikus and those poems at the end of the list that end up smaller than 17 syllables
(defn groom [haikuified]
	(let [better (into [] (map (fn [poem] (remove (fn [pair] (= (get pair :syllables) 0)) poem)) haikuified))]
		(into [] (remove (fn [poem] (or (empty? poem) (< (sum-syllables poem) 17))) better))))

(defn linify [all-haikus]
	(map (fn [haiku]
			(into [] 
				(vals
					(reduce (fn [lines word]
								(cond
									(< (sum-syllables (:first lines))  5) (into lines {:first  (conj (:first lines) word)})
									(< (sum-syllables (:second lines)) 7) (into lines {:second (conj (:second lines) word)})
									:else (into lines {:third (conj (:third lines) word)})))
							{:first [], :second [], :third []}
							haiku)))) 
		all-haikus))

(defn just-words [lined]
	(map (fn [poem] 
		(map (fn [line] 
			(str/join " " (map :word line)))
			poem)) 
		lined))

(defn make-a-sstring [lined]
	(str/join "\n\n" (map #(str/join "\n" %) lined)))

(defn -main [& args]
	(let [words (read-file "bible.txt")]
		(let [haikus (groom (haikuify (into [] (map (fn [word] {:word word :syllables (syll-count word)}) words))))]
			(let [lined (linify haikus)]
				(println (make-a-sstring (just-words lined)))))))

