(def b [1 32 "foo"])
(def a ["bar" b])
; foo
(rest(rest(first(rest a))))
; bar
(first a)

(def foobar [(first(rest(rest(first(rest a)))))
             (first a)])

; concat
(def foobarc (apply str foobar))

; a function that returns a function
(defn jaddx [x] (fn [y] (+ x y)))
; so
((jaddx 5) 10)
; => 15

; unnamed function mapped over a vec
(map #(+ %1 3) [1 2 3 4 5])
; sum evens - immutable data
(reduce + (range 0 102 2))
; loop/recur
(defn factorial 
    ([n] 
        (factorial n 1)) 
    ([n acc] 
        (if  (= n 0)   acc 
             (recur (dec n) (* acc n)))))

(defn incr [n]
    (range 2 (inc n)))
; simpler
(defn factorial_sim [n]
    (reduce * (range 2 (inc n))))

; let - binds for local use
; requires javax.vecmath.Color3f..leiningen
(let [g (+ 0.2 (rand 0.8))] 
  (Color3f. g g g))

; doto -returns the object after applying several calls
(doto (javax.swing.JFrame.)
  (.setLayout (java.awt.GridLayout. 2 2 3 3))
  (.add (javax.swing.JTextField.))
  (.add (javax.swing.JLabel. "Enter some text"))
  (.setSize 300 80)
  (.setVisible true))

; refs - akin to a pointer, mutate via a transaction
(def r (ref nil))
(dosync (ref-set r 5))
@r

; agents - refs that are modified by functions asynchronously
(def a (agent 1))
(send a inc)
(await a)
@a
(send a #(+ %1 5))
(await a)
@a

; atoms - refs that are modified by functions sychronously
(def a (atom 1))
(swap! a inc)

;; sandbox
(let [x 1 y 3 x y] x)
;; & followed by a binding-form binds to the remainder
;; :as binds the entire sequence
(let [[a b c & d :as e] [1 2 3 4 5 6 7]]
    [a b c d e])
;; => [1 2 3 (4 5 6 7) [1 2 3 4 5 6 7]]

;Array Maps - sets
(clojure.set/index [{:a 1 :b 2} {:a 1 :b 4}] [:a :b])
(clojure.set/select odd? #{1 2 3 4 5})
(:b {:a 1, :b 2} )
