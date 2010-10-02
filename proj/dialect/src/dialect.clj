(ns dialect
  (:require [clojure.contrib.string :as str]))
(defn canadianize [sentence] (str sentence ", eh"))
;"ig-Pay atinize-Lay s-Iay oming-Cay oon-Say!"
(defn pig-latinize [sentence] 
  (str/map-str 
    str
    (str/partition #" " sentence)))