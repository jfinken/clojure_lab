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

(defn update-coord
  "Returns a new ClusterPoint with coordinate at k updated to val"
  [point k val]
  (assoc point :coords (assoc (:coords point) k val)))

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
; initialization routines 
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
  "With an initialized U matrix, update the cluster index of the given point"
  [cpoint i]
  (let [max (ref -1)]
    (doseq [j (range (count @clusters))]
      (if (< @max (val-colrow-of-vec j i (count @clusters) @U))
        ; do two things: set max to Uij and update cluster index
        (do
          ; alter max
          (dosync (ref-set max (val-colrow-of-vec j i (count @clusters) @U)))
          ; update cluster index for the point
          (if (== @max 0.5)
            (dosync (ref-set data-points (assoc @data-points i (update-cluster-index cpoint 0.5))))
            (dosync (ref-set data-points (assoc @data-points i (update-cluster-index cpoint j))))))
        nil))))

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
; algorithm routines
;------------------------------------------------------------------------------
(defn euler-distance
  "Return the distance between the point and cluster centroid"
  [point centroid]
  (loop [sum 0.0 i 0]
    (if (> i (- (dimension point) 1))
      (Math/sqrt sum)
      (recur 
        (+ sum (Math/pow (- ((:coords point) i) ((:coords centroid) i)) 2))
        (inc i)))))

(defn summed-cluster-distance
  "Helper routine for calculate-objective-function"
  [cpoint i]
  (loop [sum 0.0 j 0]
    (if (> j (- (count @clusters) 1))
      sum
      (recur
        (+ sum (* (Math/pow (val-colrow-of-vec j i (count @clusters) @U) @fuzzy)
                  (Math/pow (euler-distance cpoint (@clusters j)) 2)))
        (inc j)))))

(defn calculate-objective-function
  "Each iteration is based on minimizing this function.  The function represents
   the distance from any given data point to a cluster center weighted by that 
   data point's membership grade (U matrix)"
  []
  (loop [jk 0.0 i 0]
    (if (> i (- (count @data-points) 1))
      jk
      (recur
        (+ jk (summed-cluster-distance (@data-points i) i))
        (inc i)))))

(defn calc-cluster-center-numer
   [centroid j]
   (let [uC (ref [])]
    (loop [uu 0 side-effect nil i 0]
      (if (> i (- (count @data-points) 1))
        uC 
        (recur
          ; seems sketchy: updating uu... 
          (Math/pow (val-colrow-of-vec j i (count @clusters) @U) @fuzzy)
          ; and using it in the very next form
          (doseq [k (range (dimension centroid))]
            (dosync (ref-set uC (assoc @uC k (* uu ((:coords centroid) k))))))
          (inc i))))))

(defn calc-cluster-center-denom
  [centroid j]
  (loop [l 0 i 0]
    (if (> i (- (count @data-points) 1))
      l
      (recur
        (+ l (Math/pow (val-colrow-of-vec j i (count @clusters) @U) @fuzzy))
        (inc i)))))
(defn calculate-cluster-centers
  "Do just that, calculate new cluster centers"
  []
  (doseq [j (range (count @clusters))]
    (let [centroid (@clusters j)]
      (let [uC (calc-cluster-center-numer centroid j)
            l  (calc-cluster-center-denom centroid j)]
        (doseq [k (range (dimension centroid))]
          ; update the coordinates for the centroid
          (dosync (ref-set clusters (assoc @clusters j (update-coord centroid k
                                                        (/ (uC k) l)))))
          )))))
          
      
;------------------------------------------------------------------------------
; Client code
;------------------------------------------------------------------------------
; print cluster points
(defn print-points
  []
  (doseq [i (range (count @data-points))]
    (println "point" i "cluster-index" (:cluster-index (@data-points i)) "location:" (:coords (@data-points i)))))

(defn print-clusters
  []
  (doseq [i (range (count @clusters))]
    (println "cluster" i "location: " (:coords (@clusters i)))))

; Generate random points 
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
(print-points)
(print-clusters)
