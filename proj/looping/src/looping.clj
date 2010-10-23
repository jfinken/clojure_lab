(defn min-1
  [x & more]
  (loop [min x
         more (seq more)]
	    (if-let [x (first more)]
	      (recur (if (< x min) x min) (next more))
	      min)))

; destructuring
(defn min-2
  [x & more]
  (loop [min x
         [x & more] (seq more)]
    (if x
      (recur (if (< x min) x min) more)
      min)))

;; for example:
	(let [[x & more] [1 2 3]] {:x x, :more more})
;	-> {:x 1, :more (2 3)}

	(let [coll [1 2 3]] {:x (first coll), :more (next coll)})
;    -> {:x 1, :more (2 3)};; reduce over a function as opposed to explicit looping
(defn min-of-two [fst sec]
  (if (< fst sec) fst sec))

(defn min-3
  [& coll]
  (reduce min-of-two coll))

; or
(defn min-4
  [& coll]
  (reduce 
    (fn [fst sec] (if (< fst sec) fst sec))
    coll))
    
; zipm takes sequences of keys and values to make a map:
(zipm [:a :b :c] [1 2 3])
-> {:c 3, :b 2, :a 1}

; uses loop, assoc, first, and next
; ks and vs get smaller, m gets bigger
(defn zipm-1
  [keys vals]
  (loop [m {} 
         ks (seq keys) 
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) (next ks) (next vs))
      m)))


; zipm-2 that destructures into the key and value collections, 
; avoiding all the calls to first and next.
(defn zipm-2
  [keys vals]
  (loop [m {}
         [k & ks] (seq keys)
         [v & vs] (seq vals)]
    (if (and k v)
      (recur (assoc m k v) ks vs)
      m)))

; reduces over a function associng in a key/value pair.
(defn zipm-3
  [keys vals]
  (reduce (fn [m [k v]] (assoc m k v))
    {} (map vector keys vals)))

; zipm-4 using hash-map and interleave.
; (hash-map :a 1 :b 2 :c 3)
;	-> {:a 1, :c 3, :b 2}
; (interleave [:a :b :c] [1 2 3])
;	-> (:a 1 :b 2 :c 3)

(defn zipm-4 
  [keys vals]
  (apply hash-map (interleave keys vals)))

; zipm-5 that pours the key/values into a map
(defn zipm-5
	  [keys vals]
	  (into {} (map vector keys vals)))

; Create a minmax-1 using a loop. This will look almost like min-2 from 
; the earlier step, except that it will loop over three variables instead of two:

;   1. min holds the current min
;   2. max holds the current max
;   3. more hold the remainder of the collection

; The return value should put both min and max into a map

(defn minmax-1
  [x & more]
  (loop [min x
         max x
         [x & more] (seq more)]
    (if x 
      (recur 
        (if (< x min) x min) 
        (if (> x max) x max) 
        more)
      {:min min :max max})))

; For comparison, implement minmax-2 using reduce. Think 
; carefully about the reduction function. It needs to hold 
; two values at each iteration: one each for min and max.
(defn minmax-2
  [x & more]
  (reduce
   (fn [result x]
     (->> result
         (merge-with min {:min x})
         (merge-with max {:max x})))
   {:min x :max x}
   more))
         
         
         
  