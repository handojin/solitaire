(ns solitaire.core
  (:require [clojure.string :as s])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def deck (shuffle (concat (range 1 53) '(\A \B))))

;;for testing
(def deck (vec (concat (range 1 53) '(\A \B))))

(def joker 53)

(defn- generate-char-groups 
  "step 1. - split the message text into five character groups dropping whitespace"
  [text]
  ;; make sure the message exists
  (assert (> (count text) 0) "you must provide a message to encrypt")
  
  (->
   text
   ;; eliminate whitespace
   (s/split #"\s+")
   (s/join)
   ;; convert string to a vector of chars
   (vec)
   ;; right pad vector with X chars 
   (concat (vec (repeat 4 \X)))
   ;; partition to yield char groups
   (->> 
    (partition 5))))

(defn- current-index 
  "util - get the location of the specified joker"
  [k deck]
  (.indexOf deck k))

(defn- new-index
  "util - get the new joker location"
  [k i deck]
  (let [c (- (count deck) 1)
        p (cond 
            (= k \A) (do (mod (+ i 1) c))
            (= k \B) (do (mod (+ i 2) c)))]             
    (if (= p 0) c p)))

(defn- remove-joker 
  "util - pull the joker from the deck"
  [i deck]
  (let [head (subvec deck 0 i)
        tail (subvec deck (+ i 1))]
    (vec (concat head tail))))

(defn- replace-joker 
  "util - replace the joker in the new position"
  [k n deck]
  (let [head (subvec deck 0 n)
        tail (subvec deck n)]
    (vec (concat head [k] tail))))

(defn- move-joker 
  "step 2a & 2b - general function to move the 'A' and 'B' jokers"
  [k deck]
  (let [i (current-index k deck)
        n (new-index k i deck)
        deck (remove-joker i deck)
        deck (replace-joker k n deck)
        ]
    (vec deck)))

(defn- triple-cut 
  "step 2c. - triple cut of the deck"
  [deck]
  (let [a (current-index \A deck)
        b (current-index \B deck)
        head-joker (if (< a b) a b)
        tail-joker (if (> a b) a b)

        head (subvec deck 0 head-joker)
        body (subvec deck head-joker (+ 1 tail-joker))
        tail (subvec deck (+ 1 tail-joker))]
    (vec (concat tail body head))
))

(defn- joker? 
  "util - is this card a joker?"
  [card]
  (or (= card \A) (= card \B)))

(defn- count-cut 
  "step 2d. - count cut of the deck"
  [deck]
  (let [n (last deck)]
    (if (joker? n)
      deck 
      (do (let [head (subvec deck 0 n)
                tail (subvec deck n (- (count deck) 1))]
            (vec (concat tail head [n])))))))

(defn- output
  "get the output card and current deck"
  [deck]
  (let [card (first deck)]
    (if (joker? card) 
      [(nth deck joker) deck]
      [(nth deck card) deck])))

(defn- solitaire-turn
  "step 2. -  perform solitaire algorithm to generate a deck"
  [deck]
  (->> 
   deck
   (move-joker \A)
   (move-joker \B)
   (triple-cut)
   (count-cut)))

(defn- generate-keystream [deck]
  (let [[k d] (output (solitaire-turn deck))]
    ;;(println k d)
    (filter number? 
            (lazy-seq (cons k (generate-keystream d))))))

(defn- char->int [char-groups]
  (let [offset 64]
    (map #(- (int %) offset) (flatten char-groups))))

(defn- int->char [numbers]
  (let [offset 64]
    (map #(char (+ (if (= % 0) 26 %) offset)) numbers)))

(defn prepare-message [message]
  (->> message 
       (s/upper-case)
       (generate-char-groups)
       (char->int)))

(defn format-output [message]
  (->> message
      (int->char)
      (partition 5)
      (map #(apply str %))
      (s/join " ")))

(defn encrypt [message deck]
  (let [message (prepare-message)
        n (count message) 
        message (map #(mod (+ %1 %2) 26) message (take n (generate-keystream deck)))]
    (format-output message)))

(defn decrypt [message deck]
  (let [message (s/upper-case message)
        message (generate-char-groups message)
        message (char->int message)
        n (count message)
        message (map #(mod (- %1 %2) 26) message (take n (generate-keystream deck)))]
    (format-output  message)))
