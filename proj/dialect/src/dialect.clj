(ns dialect
  (:require [clojure.contrib.string :as s]))
(defn canadianize [sentence] (str sentence ", eh"))

; playing with type hinted string - unused here
(defn str-len [^String x]
  (.length x))

(defn pig-latin [word]
  ;take first letter, capitalize, add to end with '-ay'
  (str 
    (s/tail (- (.length word) 1) word)
    "-"
    (s/upper-case (str (get word 0)))
    "ay "))
  
(defn pig-latinize [sentence] 
  (s/chop
    (s/map-str 
      pig-latin    
      (.split sentence " "))))
