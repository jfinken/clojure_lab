(def a (/ 1.0 3.0))
(def b (/ 3.0 1.0))
(def c (* a a a a a a a a a a))
(def d (* b b b b b b b b b b))

; ratios
(def a1(/ 1 3))
(def b1(/ 3 1))
(def c (* a1 a1 a1 a1 a1 a1 a1 a1 a1 a1))
(def d (* b1 b1 b1 b1 b1 b1 b1 b1 b1 b1))

(bit-and 2r1100 2r0100)
(Integer/parseInt "101010" 2)

;structs
(defstruct employee :name :id)
(struct employee "StudBoy", 19)
(def studboy (struct-map employee :id 19 :name "StudBoy"))

; accessor
(def s-name (accessor employee :name))
; assoc and dissoc
(def studboy1 (assoc studboy :function "cool engineer"))
(def studboy2 (dissoc studboy1 :function "cool engineer"))
; update it, but the struct is immutable
(assoc studboy :name "Spudz Boy")
; bind
(def spudz (assoc studboy :name "Spudz"))

; exception handling
(try (/ 1 0)
  (catch ArithmeticException e (prn "in catch"))
  (finally (prn "in finally")))

;collections
(def b (12 56 "foo"))

