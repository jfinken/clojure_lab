(ns log_parser.core
  (:require [clojure.string :as string])
  (:import [java.io File])
  (:require [clojure.contrib.duck-streams :as duck])
  (:require [clojure.contrib.http.agent :as http])
  (:use incanter.core)
  (:use incanter.charts)
  )

;------------------------------------------------------------------------------
; utilities
;------------------------------------------------------------------------------
(defn get-res
  [in-str pattern]
  "Return a string from in-str that matches the re pattern.  NOTE: cannot be used
  with 'map' because f applied to the map takes number-of-colls arguments."
  (first
    (re-seq
      pattern
    in-str)))
(defn re-pos 
  "Return the positions and matches of a regex in the string"
  [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

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
;------------------------------------------------------------------------------
; api call processing  
;------------------------------------------------------------------------------
(defn get-api-call
  "If it exists, returns the string api method in the given string"
  [in-str]
  (first
    (re-seq
      #"\bget-[A-Za-z]+-*[A-Za-z]*-*[A-Za-z]*\b"
    in-str)))

(defn get-all-api-calls
  "Returns a collection of all found api-calls in an ASCII file."
  [filename]
  (map 
    get-api-call
    (duck/read-lines filename)))

(defn get-api-call-freqs
  [filename]
  (frequencies 
    (get-all-api-calls filename)))

(defn get-all-api-call-freqs-m
  "Merges api-call frequency maps from each parsed file."
  [log-dir]
    (loop
      [ret {} 
       maps (map get-api-call-freqs 
              (walkr log-dir #".*\.access.log"))]
      (if-let [mp (first maps)]
        (recur (merge-with + ret mp) (next maps))
        ret)))


(def temp "INFO [com.bitgirder.servlet.ServiceServlet]: 82.216.88.111, Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1) ; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729), zgc6ypp4bz5drttcc524phnq [Sun, 27 Mar 2011 17:37:32 +0000] get-plane-pano, 73 ms")
;------------------------------------------------------------------------------
; this will parse out the duration of the api call from the line. 
; TO-DO: generate a map with averaged times, i.e {"get-panoramas" 13}
;------------------------------------------------------------------------------
(assoc {}
  (get-res (subs temp (first (keys (re-pos #"\bget-\b" temp))))
    #"\bget-[A-Za-z]+-*[A-Za-z]*-*[A-Za-z]*\b")
  (Integer/parseInt
    (get-res (subs temp (first (keys (re-pos #"\bget-\b" temp))))
         #"\b[0-9]+\b")))

;------------------------------------------------------------------------------
; get the api-key
;------------------------------------------------------------------------------
(defn get-api-key
  "If it exists, returns the 24-char api-key string in the given string"
  [in-str]
  (first
    (re-seq
      #"\b[A-Za-z0-9]{24}\b"
      in-str)))
;------------------------------------------------------------------------------
; IP address processing
;------------------------------------------------------------------------------
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
; essentially, main:
;------------------------------------------------------------------------------
(def log-dir "/Users/josh/projects/clojure_lab/proj/log_parser/data")

(defn get-all-calls 
  "Returns a frequency map, with nil removed, of all api-calls
   parsed from any access log files within the given directory."
  [dir]
  (apply merge 
    (for [[k v] 
        (seq (get-all-api-call-freqs-m dir))
        :when (not (nil? k))] {k v})))

; if you want them sorted
(def sorted-calls 
  (into 
    (sorted-map-by 
      (fn [key1 key2] (<= (calls key2) (calls key1))))
    calls))

(def calls (get-all-calls log-dir))
(view (pie-chart (keys calls) (vals calls)
       :title "api calls"
       :legend true))

; records is now something like:
; ([nil 27] ["10.63.125.69" 2] ["186.213.21.148" 12] ["189.158.49.211" 8])

; to get the max key-val record
;(reduce max-of-two-vecs records)
; remove nil keys - incanter isn't down with nil keys
(def records (seq (get-all-ip-freqs-m log-dir)))
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
(def max-hits 5)
(def locations
    (map get-location (take max-hits (keys ips2))))

(view (bar-chart (take max-hits (keys ips2))
                  (take max-hits (vals ips2))
                 :title "IP Addresss/Requests"
                 :x-label "IP Address"
                 :y-label "Requests"))


; post to get the lat, lng of the ip...bit of a hack
(defn get-location
  [ip-addr-str]
  (get-google-lat-lng
    (http/string (http/http-agent "http://www.ip-address.org/lookup/ip-locator.php" 
                              :method "POST" 
                              :body (str "ip=" ip-addr-str)))))
(defn get-google-lat-lng
  "If it exists, returns the string lat, lng within the given string.
  Mega-hack because it is completely tailored to a specific response."
  [in-str]
  (first
    (string/split
      (second
        (string/split
          in-str
          #"\bnew google.maps.LatLng\(\b"
          ))
      #"\)")))
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

