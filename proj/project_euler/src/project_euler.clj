
;;------------------------------------------------------
;; For problem 1 
;;------------------------------------------------------
(defn divides-j?
  "takes a dividend and a divisor, and returns true if 
   divisor evenly divides the dividend"
  [dend visor]
  (if (zero? (mod (float dend) (float visor)))
    true
    false))

;; more succinct
(defn divides?
  "Does divisor divide dividend evenly?"
  [dividend divisor]
  (zero? (rem dividend divisor)))

; We will eventually want to filter on divisibility by more 
; than one number, so create a divides-any function that takes 
; a variable list of numbers, and returns a predicate that tests 
; whether its arg can be evenly divided by any of the numbers. 
; (Hint: work inside-out, using divides, some, and boolean). 
; btw, A predicate is a function that returns either true or false

; Here, the pound sign, followed by an expression. The expression becomes 
; the body of the function, and any percent signs in the body are 
; interpreted as arguments to the function.
(defn divides-any
  [& divisors]
  ; return predicate
  (fn [dividend-arg]
    (boolean (some #(divides? dividend-arg %) divisors))))

; recursive solution to problem 1 of project euler
;   1. let a local function divisible? that tests for divisibility by 3 or 5.
;   2. Loop starting with a sum of zero and an n of one.
;   3. The loop should terminate if n is greater than or equal to upper.
;   4. The recur should always increment n. If n is divisible?, sum should increment 
;      by n, otherwise, sum should remain unchanged.
(defn add-below-divisor
  [upper]
  (let [divisible? (divides-any 3 5)]
    (loop [sum 0
           n 1]
      (if (>= n upper)
        sum
        (recur (if (divisible? n) (+ sum n) sum) (inc n))))))
; run it as:
(add-below-divisor 1000)
; same using reduction:
(defn add-below-divisor-2
  "Sum the numbers divisible by 3 or 5, from 0 to upper."
  [upper]
  (reduce + (filter (divides-any 3 5) (range upper))))

; The Clojure macros -> and ->>  take their first argument, and insert it into the 
; second form. The result of the second form is inserted into the third form, and 
; so on. -> inserts arguments at the first position, and ->> inserts at the last position.
; Again using -> and ->> macros
(defn add-below-divisor-3
  "Sum the numbers divisible by 3 or 5, from 0 to upper."
  [upper]
  (->> (range upper)
    (filter (divides-any 3 5))
    (apply +)))

;;------------------------------------------------------
;; For problem 2
;; "omg, CIA agents are supposed to distribute crack to
;; the inner city not smoke it."
;;------------------------------------------------------
(defn fib
  " return fibonacci out to max-terms"
  [max-terms]
  (loop [fseq (seq [1 2])
         sum-evens 0]
    (if (> (count fseq) max-terms )
      ;sum-evens
      fseq
      (recur (concat fseq 
               (vector
                 (+ 
                   (last fseq)
                   (nth fseq (- (count fseq) 2)))))
        (if (even? (last fseq))
          (+ sum-evens (last fseq))
          sum-evens)
        ))))
               
(defn fib-v
  " return fibonacci out to max-value, summing the evens"
  [max-value]
  (loop [fseq (seq [1 2])
         sum-evens 0]
    (if (> (last fseq) max-value)
      ;sum-evens
      (butlast fseq)
      (recur (concat fseq 
               (vector
                 (+ 
                   (last fseq)
                   (nth fseq (- (count fseq) 2)))))
        (if (even? (last fseq))
          (+ sum-evens (last fseq))
          sum-evens)
        ))))
  
;;------------------------------------------------------
;; Problem 6
;; The sum of the squares of the first ten natural numbers is,
;; 1^(2) + 2^(2) + ... + 10^(2) = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
;; 
;; Find the difference between the sum of the squares of 
;; the first one hundred natural numbers and the square of the sum.
;;------------------------------------------------------
(defn sq-of-sum
  "sqaure the sum of the first n numbers"
  [n]
  (let[ sum (reduce + (range 1 n))]
    (* sum sum)))
 
(defn sum-of-sq
  "sum of the squares of the first n numbers"
  [n]
  (reduce + (for [x (range 1 n)] (* x x))))

(defn sq-sum-diff
  [n]
  (- (sq-of-sum n) (sum-of-sq n)))

;;------------------------------------------------------
;; Required for problems 3 and 5
;;------------------------------------------------------
(defn is-prime
  "return true if the number is prime"
  [n]
  (if (> n 3)
    (if (zero? (rem n 2))
      false
      (if (zero? (rem n 3))
        false
	    ; avoid considering multiples of 2 and 3.  Done easily and
	    ; simply by starting at 5 and incr by 2 and 4 alternatively, i.e.
	    ;  5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35, 37, ...
	    (loop [max-div (inc (* n n))
	           divisor 5
	           incr 2
	           ret true]
	      (if (>= divisor max-div)
	        ret
	        (recur max-div (+ divisor incr) (- 6 incr) (if (zero? (rem n divisor)) false true))
	        )
	      )))
    true))
            