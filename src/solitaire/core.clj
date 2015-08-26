(ns solitaire.core
  (:require [clojure.string :as s])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def deck (shuffle (concat (range 1 53) '(\A \B))))

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
   (concat (vec (repeat 5 \X)))
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
    deck))

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

(defn- generate-keystream 
  "step 2. - generate n keystream letters where n = length of message"
  [n]
  

)

