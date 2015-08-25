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

(defn- move-joker-A [d]
  
  )

(defn- generate-keystream 
  "step 2. - generate n keystream letters where n = length of message"
  [n]
  

)

