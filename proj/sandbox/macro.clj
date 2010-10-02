; these are equivalent
(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(defmacro dbg-1 [x] 
  (list 'let ['a x] (list 'println (list 'quote x) "=" 'a)
        'a))

;; test it out
(defn factorial [n]
    (if (< n 2) n
            (dbg-1 (* n (factorial (dec n))))))


; stuff
(defmacro addx[x] '(+ ~x 5))

(list 'let [ 2 4])

(defn printme [x]
  'println x)

