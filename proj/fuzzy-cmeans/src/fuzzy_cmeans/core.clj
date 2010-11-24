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

(defn update-cluster-index
  "Return a new ClusterPoint with the cluster-index assigned to val"
  [cpoint val]
  (assoc cpoint :cluster-index val))

; Helper fns for the U matrix
(defn val-colrow-of-vec
  [col row num-cols vec]
  (vec (+ col (* num-cols row))))
  
(defn alter-colrow-of-ref-vec
  [col row num-cols ref-vec value]
  (dosync 
    (ref-set ref-vec 
      (assoc @ref-vec (+ col (* num-cols row)) value))))


;------------------------------------------------------------------------------
; init fuzzy c-means
; - going to need global refs for points, clusters, fuzzy and the U matrix
;------------------------------------------------------------------------------
(def data-points (ref []))
(def clusters (ref []))
(def U (ref []))
(def fuzzy (ref -1))
(def eps (Math/pow 10 -5)) ; algorithm precision

; sum a vector
(defn sum-vec
  "Simply sum the contents of a vec"
  [vec]
  (apply + vec))

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
  "Part 1 of 3 in the initialization of the U matrix"
  [cpoint i]
  (doseq [j (range (count @clusters))]
    (alter-colrow-of-ref-vec j i 
      (count @clusters) U (euclid-dist-2d-or-eps cpoint (@clusters j)))
    ))

(defn init-U-matrix-2
  "Part 2 of 3 in the initialization of the U matrix"
  [cpoint i]
 (let [curr-sum (sum-vec @U)]
   (doseq [j (range (count @clusters))]
     (alter-colrow-of-ref-vec j i
       (count @clusters) U 
       ; value to stuff into U
       (/ 1.0 (Math/pow (/ (val-colrow-of-vec j i (count @clusters) @U) curr-sum) (/ 2.0 (- @fuzzy 1.0)))))
     )))

(defn init-U-matrix-3
  "Part 3 of 3 in the initialization of the U matrix"
  [cpoint i]
  (let [curr-sum (sum-vec @U)]
    (doseq [j (range (count @clusters))]
      (alter-colrow-of-ref-vec j i
        (count @clusters) U
        ; value to stuff into U
        (/ (val-colrow-of-vec j i (count @clusters) @U) curr-sum)))
    ))

(defn recalc-cluster-index
  "Recalculate the cluster index of the given point"
  [cpoint i]
  (loop [max -1.0 j (count @clusters) p nil]
    (if (zero? j)
      nil
      (recur 
        (if (< max (val-colrow-of-vec j i (count @clusters) @U))
          (val-colrow-of-vec j i (count @clusters) @U)
          max)
        (dec j)
        ; side-effect, update the point's cluster index
        nil
        ;(if (== max 0.5)
        ;  (dosync (ref-set data-points (assoc @data-points i (update-cluster-index cpoint 0.5))))
        ;  (dosync (ref-set data-points (assoc @data-points i (update-cluster-index cpoint j)))))
        ))))

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
  (doseq [i (range (count @data-points))]
    (init-U-matrix-1 (@data-points i) i)
    (init-U-matrix-2 (@data-points i) i)
    (init-U-matrix-3 (@data-points i) i))
  
  ; recalculate cluster indices
  (doseq [i (range (count @data-points))]
    (recalc-cluster-index (@data-points i) i))
  
  )
;------------------------------------------------------------------------------
; Client code
;------------------------------------------------------------------------------

; tested
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

; sample, right off the bat
(def in-fuzzy 2.0)
(def xmin 100)
(def xmax 500)
(def ymin 100)
(def ymax 500)
(def num-clusters 5)
(def pts (gen-cluster-points 10 xmin xmax ymin ymax))
(def centroids (gen-cluster-points num-clusters xmin xmax ymin ymax))
; init!
(init-cmeans pts centroids in-fuzzy)