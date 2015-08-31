(ns solitaire.core
  (:require [clojure.string :as s])
  (:gen-class))

;;maybe use later
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def deck (vec (shuffle (concat (range 1 53) '(\A \B)))))

(def joker 53)

;;for testing
(def deck (vec (concat (range 1 53) '(\A \B))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prepare-message 
  "prepare text for encrypt/decrypt"
  [message]

  ;; perform sanity checks
  (assert (> (count message) 0) "you must provide a message to encrypt")
  (assert (not (re-matches #"\s+" message)) "message cannot consist solely of whitespace")
  (assert (re-matches #"[A-Za-z\s]+" message) "message cannot contain non-alpha characters")
  
  (let [message (s/replace (s/upper-case message) #"\s+" "")
        message (concat message (repeat 4 \X))
        message (flatten (partition 5 message))]
    (char->int  message)))                           

(defn- char->int 
  "convert a list of chars to integers"
  [char-groups]
  (let [offset 64]
    (map #(- (int %) offset) char-groups)))

(defn- int->char 
  "convert a list of integers to characters"
  [numbers]
  (let [offset 64]
    (map #(char (+ (if (= % 0) 26 %) offset)) numbers)))

(defn- current-index 
  "get the location of the k specified joker"
  [k deck]
  (.indexOf deck k))

(defn- new-index
  "get the new location of the k specified joker"
  [k i deck]
  (let [c (- (count deck) 1)
        p (cond 
            (= k \A) (do (mod (+ i 1) c))
            (= k \B) (do (mod (+ i 2) c)))]             
    (if (= p 0) c p)))

(defn- remove-joker 
  "pull the joker from the deck"
  [i deck]
  (let [head (subvec deck 0 i)
        tail (subvec deck (+ i 1))]
    (vec (concat head tail))))

(defn- replace-joker 
  "replace the joker in the new position"
  [k n deck]
  (let [head (subvec deck 0 n)
        tail (subvec deck n)]
    (vec (concat head [k] tail))))

(defn- joker? 
  "util - is this card a joker?"
  [card]
  (or (= card \A) (= card \B)))

(defn- format-output 
  "format output to five character groups as tradition dictates"
  [message]
  (->> message
       (int->char)
       (partition 5)
       (map #(apply str %))
       (s/join " ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;core solitaire functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- move-joker 
  "general function to move the 'A' and 'B' jokers appropriately"
  [k deck]
  (let [i (current-index k deck)
        n (new-index k i deck)
        deck (remove-joker i deck)
        deck (replace-joker k n deck)]
    (vec deck)))

(defn- triple-cut 
  "perform a  triple cut of the deck"
  [deck]
  (let [a (current-index \A deck)
        b (current-index \B deck)
        head-joker (if (< a b) a b)
        tail-joker (if (> a b) a b)

        head (subvec deck 0 head-joker)
        body (subvec deck head-joker (+ 1 tail-joker))
        tail (subvec deck (+ 1 tail-joker))]
    (vec (concat tail body head))))

(defn- count-cut 
  "perform a count cut of the deck"
  [deck]
  (let [n (last deck)]
    (if (joker? n)
      deck 
      (do (let [head (subvec deck 0 n)
                tail (subvec deck n (- (count deck) 1))]
            (vec (concat tail head [n])))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;solitaire algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- solitaire-turn
  "step 2. -  perform a turn of the solitaire algorithm to generate a deck"
  [deck]
  (->> 
   deck
   (move-joker \A)
   (move-joker \B)
   (triple-cut)
   (count-cut)))

(defn- output-key
  "get the output card and current deck"
  [deck]
  (let [card (first deck)]
    (if (joker? card) 
      [(nth deck joker) deck]
      [(nth deck card) deck])))

(defn- generate-keystream 
  "generate a lazy stream of successive turns of the solitaire alglorithm"
  [deck]
  (let [[k d] (output-key (solitaire-turn deck))]
    (filter number? (lazy-seq (cons k (generate-keystream d))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encrypt 
  "encrypt a message"
  [message deck]
  (let [message (prepare-message message)
        n (count message) 
        message (map #(mod (+ %1 %2) 26) message (take n (generate-keystream deck)))]
    (format-output message)))

(defn decrypt 
  "decrypt a message"
  [message deck]
  (let [message (prepare-message message)
        n (count message)
        message (map #(mod (- %1 %2) 26) message (take n (generate-keystream deck)))]
    (format-output  message)))
