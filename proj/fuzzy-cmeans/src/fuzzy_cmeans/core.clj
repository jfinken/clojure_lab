(ns fuzzy-cmeans.core)

;------------------------------------------------------------------------------
; generating/working with a 2D array
;------------------------------------------------------------------------------
(into-array (map double-array [[1 2] [3 4]]))

(defn double-array-2d [coll]
  (let [w (count coll)
        h (apply max (map count coll))
        arr (make-array Double/TYPE w h)]
    (doseq [x (range w)
            y (range h)]
      (aset arr x y (double (get-in coll [x y]))))
    arr))

;; too complicated, what about a 1-d vector and sugar to
;; map 2d indexing into it.
;; i.e. for a cols x rows, m x n matrix:
;; (i, j) => col + (row * num_cols) => i + m*j
(def u (ref [2 4 6 8 10 12 14 16 18 20 22 24]))

(defn ij-of-vec
  [i, j, m, n, vec]
  (vec (+ i (* m j))))
  
;=> (ij-of-vec 2 2 4 3 @u)
;22
;=> (ij-of-vec 1 2 4 3 @u)
;20
; assignment
; (assoc [1 2 3] 3 45)

;------------------------------------------------------------------------------
; Cluster Point definition (map)
; - vector of type double of size N for the N dimensional data
; - cluster index, double (fuzzy)
;------------------------------------------------------------------------------
;(def cluster-point {:coords [] :cluster-index -1}) 
;; - or -
(defstruct cluster-point :coords :cluster-index)
; in case the interface changes
(def coords (accessor cluster-point :coords))
(def get-index (accessor cluster-point :cluster-index))
(defn coords-at
  "Given an instance of cluster-point and an index, 
  return the value of :coords of the cluster-point at the index."
  [pt index]
  ((coords pt) index))
(defn dimension
  "Dimensions of the cluster-point data"
  [point]
  (count (coords point)))
(defn get-cluster-index
  "Given an instance of cluster-point, return its current
  cluster index"
  [point]
  (get-index point))
(defn set-cluster-index
  "Given an instance of cluster-point, set or replace the cluster-index"
  [point val]
  (assoc point :cluster-index val))

;; for example 
(def pt (struct-map cluster-point :coords [238 125] :cluster-index -1))
(defn build-point
  "Builds a cluster-point given its dimensional data"
  [coord-data]
  (struct-map cluster-point :coords coord-data :cluster-index -1))
(def pt (build-point [37 456 69]))

;------------------------------------------------------------------------------
; init fuzzy c-means
; - going to need global refs for points, clusters, fuzzy and the U matrix
;------------------------------------------------------------------------------
(def data-points (ref []))
(def clusters (ref []))
(def U (ref []))
(def fuzzy (ref -1))
(defn init-cmeans
  "Init the algorithm with a list of cluster-points, a list of cluster-points
  representing the initial number of clusters and their centroids, and an initial
  fuzzy value.
  
  A large fuzzy value results in smaller memberships Uij and hence fuzzier clusters"
  [in-points in-clusters in-fuzzy]
  ; stash
  (dosync (ref-set data-points in-points))
  (dosync (ref-set clusters in-clusters))
  (dosync (ref-set fuzzy in-fuzzy))
  
  ; loop over the points and clusters to create the initial U matrix
  
  )