(ns update-model.core
  (:refer-clojure :exclude [get]))

; Use atoms to build a simple object cache. 
; First, create a create-1 function that returns an atom wrapped around an empty map.
(defn create-1
  []
  (atom {}))

(defn create
  "more user friendly create"
  ([] (atom {}))
  ([m] (atom m)))

(defn get
  "returns an item from the object cache"
  [cache key]
  (@cache key))
    
(defn put-1
  "put an item into the atomic cache"
  [cache key val]
  ;(reset! cache (assoc @cache key val))) ;hammer rather than a scapel
  (swap! cache assoc key val))

(defn put
  "can take a key value pair or an entire map to be added"
  ([cache key val] (swap! cache assoc key val))
  ([cache entire-map] (swap! cache conj entire-map)))
  
(let [c (create {:josh 34})] 
  (put c :bryce 29)
  (put c {:oliver 16, :tom 31}))