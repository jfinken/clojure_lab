(ns log_parser.core
  (:require [clojure.string :as string])
  (:import [java.io File])
  (:require [clojure.contrib.duck-streams :as duck]))

(defn get-ip-addr
  "If it exists, returns the string IP address in the given string"
  [in-str]
  (first
    (re-seq
    #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b"
   in-str)))

(defn map-get-all-ip
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


(def log-dir "/Users/josh/projects/clojure_lab/proj/log_parser/data")
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

; works:
(def v (seq (get-all-ip-freqs log-dir)))
(reduce max-of-two-vecs v)

; buggy
(defn max-val-2
  [& in-map]
  ; make it a seq of 2-elem vectors
  (let [keyvals (seq in-map)
        m (reduce max-of-two-vecs keyvals)])
    m)



;---------------
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

