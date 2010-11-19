(ns fuzzy-cmeans.core)

;------------------------------------------------------------------------------
; generating/working with a 2D array
;------------------------------------------------------------------------------
(comment
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
)


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

; methods for the old cluster-point as a struct map
(comment 
	(defstruct cluster-point :coords :cluster-index)
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
)


;------------------------------------------------------------------------------
; ClusterPoint definition using datatypes and protocols 
; - to get the cluster-index for example: (:cluster-index my-point)
; - to update it: (assoc pts 0 (assoc (pts 0) :cluster-index 0.12345))
;   where pts is a seq of ClusterPoints
;------------------------------------------------------------------------------
(defprotocol cluster-point
  "protocol for cluster-points"
  (coords-at [point indx])
  (dimension [point]))

(defrecord ClusterPoint [coords cluster-index]
  cluster-point
  (coords-at [this indx] (coords indx))
  (dimension [this] (count coords)))

; Helper fns for the U matrix
(defn val-ij-of-vec
  [i j num-cols vec]
  (vec (+ i (* num-cols j))))
  
(defn alter-ij-of-ref-vec
  [i j num-cols ref-vec value]
  (dosync 
    (ref-set ref-vec 
      (assoc @ref-vec (+ i (* num-cols j)) value))))


;------------------------------------------------------------------------------
; init fuzzy c-means
; - going to need global refs for points, clusters, fuzzy and the U matrix
;------------------------------------------------------------------------------
(def data-points (ref []))
(def clusters (ref []))
(def U (ref []))
(def fuzzy (ref -1))
(def eps (Math/pow 10 -5)) ; algorithm precision
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

; euclidean distance in only 2-d
(defn euclid-dist-2d-or-eps
  "Return the 2D euclidean distance of two cluster points"
  [cp1 cp2]
  (let [diff
        (Math/sqrt (+ (Math/pow (- (coords-at cp1 0) (coords-at cp2 0)) 2)
                      (Math/pow (- (coords-at cp1 1) (coords-at cp2 1)) 2)))]
    (if (zero? diff)
      eps
      diff)))
        
(defn init-U-matrix-1
  [cpoint i]
  (let [j 0
        sum 0]
    (doseq [cent (seq @clusters)]
      (alter-ij-of-ref-vec i j 
        (count @clusters) U (euclid-dist-2d-or-eps cpoint cent))
      (+ sum (val-ij-of-vec i j (count @clusters) @U)))
        ; return sum
        sum))

;------------------------------------------------------------------------------
; Client code
;------------------------------------------------------------------------------
(comment
	(defn gen-cluster-points-bunk 
	  "Return a vector of n cluster points with random coordinate values
	   within the given range"
	  [n xmin xmax ymin ymax]
	  (let [ret (vector)]
	    (doseq [m (range n)]
	      (let [x ( + (mod (rand Integer/MAX_VALUE) (inc (- xmax xmin))) xmin)
	           y ( + (mod (rand Integer/MAX_VALUE) (inc (- ymax ymin))) ymin)]
	      (conj ret (ClusterPoint. (vector x y) -1))))
	      ;(println m x y)))
	    ret))
 )

(defn gen-cluster-points
  "Return a vector of n cluster points with random coordinate values
   within the given range"
  [n xmin xmax ymin ymax]
  (loop [m n ret (vector)]
    (if (zero? m)
      ret
      (recur 
        (dec m)
        (conj ret
               (ClusterPoint. 
                 (vector 
                   ( + (mod (rand Integer/MAX_VALUE) (inc (- xmax xmin))) xmin) ;x
                   ( + (mod (rand Integer/MAX_VALUE) (inc (- ymax ymin))) ymin));y
                 -1))))))
;(gen-cluster-points 10 100 500 100 500)

(comment
; init-euclidean-sum-1
; first iteration of three in initializing a row in the U matrix
(defn init-euclidean-sum-1
  [cpoint]
  (loop [j 0
         row-of-u []
         sum 0
         [cent & more] (vec @clusters)]
    (if cent
      (recur 
        ; j
        (inc j)
        ; never really want 'diff' anyway.  you want Uij to either be diff or eps
        (assoc row-of-u j (euclid-dist-2d-or-eps cpoint cent))
        ; sum
        (+ sum (row-of-u j))
        ; remainder of clusters
        more)
      ; done. alter U
     
      )))
)

