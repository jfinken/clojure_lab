(ns fuzzy-cmeans.test.core
  (:use [fuzzy-cmeans.core] :reload)
  (:use [clojure.contrib.string])
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(defn read-points-file
  [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (reduce conj [] (line-seq rdr))))

(defn parse-point-str
  "Given a comma-separated point as a string, return
  a vector of doubles"
  [pt-str]
  (let [temp (.split pt-str ",")]
   (loop [side nil
          ret-vec (vector)
          more (seq temp)]
     (if (not (blank? (first more)))
       (recur 
         (println "first more:" (first more))
         (conj ret-vec (Double/parseDouble (first more)))
         (next more))
       ret-vec))))

(defn parse-points-file
  "Given a file where each line is a comma-sep list of dimensional
  values, construct a ClusterPoint for each"
  [path]
  (let [raw-pts-vec (read-points-file path)]
    (loop [ret-pts (vector)
           more (seq raw-pts-vec)]
      (if (boolean (seq (first more)))
        (recur (conj ret-pts (ClusterPoint. (parse-point-str (first more)) -1))
               (next more))
        ret-pts))))



