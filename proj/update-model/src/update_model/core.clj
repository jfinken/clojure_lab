(ns update-model.core
  (:refer-clojure :exclude [get])
  (:use [ring.util.codec :as codec])
  (:use [clojure.contrib.json :only (read-json)])
  (:require [http.async.client :as http]))

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

;-----------------------------------------------------------------------------
; Refs
;-----------------------------------------------------------------------------
(defn create
  "cache based on refs"
  ([] (create {}))
  ([m] (ref m)))

(defn get-r
  [cache key]
  (@cache key))

(defn put
  ([cache key val]
    (dosync (alter cache assoc key val)))
  ([cache entire-set]
    (dosync (alter cache merge entire-set))))
      
(defn fast-put
  ([cache key val]
    (dosync (commute cache assoc key val)))
  ([cache entire-set]
    (dosync (commute cache merge entire-set))))

(defn put-josh
  ([cache key val] 
    (dosync (ref-set cache (assoc @cache key val))))
  ([cache entire-map] 
    (dosync (ref-set cache (conj @cache entire-map)))))

;-----------------------------------------------------------------------------
; Futures
;-----------------------------------------------------------------------------
(def google-search-base
  "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=")

(defn gsearch
  [terms]
  (let [response (http/GET (str google-search-base (codec/url-encode terms)))]
        (http/await response)
        (http/string response)))

(defn est-hit-count
  [terms]
  (java.lang.Long/parseLong
    (:estimatedResultCount 
      (:cursor 
        (:responseData (read-json (gsearch terms)))))))