(ns log_parser.core
  (:require [clojure.string :as string])
  (:import [java.io File])
  (:require [clojure.contrib.duck-streams :as duck])
  (:use incanter.core)
  (:use incanter.charts)
  (:require [clojure.contrib.http.agent :as http])
  )

(defn get-ip-addr
  "If it exists, returns the string IP address in the given string"
  [in-str]
  (first
    (re-seq
    #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b"
   in-str)))

(defn get-all-ip
  "Returns a collection of all found IP addresses in an ASCII file."
  [filename]
  (map 
    get-ip-addr
    (duck/read-lines filename)))

(defn get-ip-freqs
  [filename]
  (frequencies 
    (get-all-ip filename)))


;(def files (file-seq (duck/file-str log-dir)))

(defn walkr
  "Given a directory, return a sequence of all files whose filenames
  match pattern"
  [dirpath pattern]
  (loop 
    [ret (vector)
     files (file-seq (duck/file-str dirpath))]
      (if-let [file (first files)]
        (recur (if (re-matches pattern (.getName file))
                 (conj ret file)
                 ret)
               (next files))
      ret)))


(defn get-all-ip-freqs
  "Return a map of IP addr frequencies for each parsed file."
  [log-dir]
    (map get-ip-freqs 
      (walkr log-dir #".*\.access.log")))

(defn get-all-ip-freqs-m
  "Merges IP addr frequency maps from each parsed file."
  [log-dir]
    (loop
      [ret {} 
       maps (map get-ip-freqs 
              (walkr log-dir #".*\.access.log"))]
      (if-let [mp (first maps)]
        (recur (merge-with + ret mp) (next maps))
        ret)))

(defn max-of-two-vecs [fst sec]
  (if (> (last fst) (last sec)) fst sec))

;------------------------------------------------------------------------------
; works:
;------------------------------------------------------------------------------
(def log-dir "/Users/josh/projects/clojure_lab/proj/log_parser/data")
(def records (seq (get-all-ip-freqs-m log-dir)))
; records is now something like:
; ([nil 27] ["10.63.125.69" 2] ["186.213.21.148" 12] ["189.158.49.211" 8])

; to get the max key-val record
;(reduce max-of-two-vecs records)
; remove nil keys - incanter isn't down with nil keys
(def ips 
  (apply merge (for [[k v] records :when (not (nil? k))] {k v})))
; ips is now a histogram map with no nil keys like:
; {"195.228.140.139" 6, "63.225.17.34" 39, "10.63.125.69" 2, "186.213.21.148" 12}

; sort by values descending
(def ips2
  (into 
    (sorted-map-by 
      (fn [key1 key2] (<= (ips key2) (ips key1))))
    ips))

; get the ip-addrs with the most 
;(def ips2 
;  (apply merge (for [[k v] ips :when (> v 5000)] {k v})))

;------------------------------------------------------------------------------
; incanter hist
;------------------------------------------------------------------------------
(view (bar-chart (keys ips2) (vals ips2)))
; get the top n ip addresses by hits
(def max-hits 7)
(view (bar-chart (take max-hits (keys ips2)) (take max-hits (vals ips2))
                 :title "IP Address/Requests"
                 :x-label "IP Address"
                 :y-label "Requests"))

;------------------------------------------------------------------------------
; to post to http://www.ipgp.net with body ip=69.181.46.108&mode=view
;------------------------------------------------------------------------------
(http/string (http/http-agent "http://www.ipgp.net"
      :method "POST" :body "ip=69.181.46.108&mode=view"
      :headers {:Content-Type "application/x-www-form-urlencoded",
                 :Referer "http://www.ipgp.net"}
))

(http/string (http/http-agent "http://www.google.com"
    :method "GET"))
;------------------------------------------------------------------------------
; prototype code below...
;------------------------------------------------------------------------------
; for example
(walkr log-dir #".*\.access.log")

(defn parse-it
  [filename]
  (get-ip-addr
    (first
      (string/split
      (last (drop 1 (duck/read-lines filename)))
      #","))))

(defn walk 
  " doseq each item in a dir, matching file name to
  pattern"
  [dirpath pattern]
    (doseq [file (-> dirpath File. file-seq)]
          (if (re-matches pattern (.getName file))
                  (println (.getPath file)))))

  ;(take-nth 2 (drop 1 (duck/read-lines filename))))
  ;(read-lines filename)

;(reduce #(assoc %1 %2 (inc (%1 %2 0))) {} (re-seq #"\w+" s))

