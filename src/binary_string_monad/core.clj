(ns binary-string-monad.core
  (:use [clojure.algo.monads :as monads]))

; Binary string to decimal number conversion
(defn binary-string-to-digits
  "Converts binary string \"1001\" to an array of digits [1 0 0 1]"
  [binary-string]
  
  (let [zero (char 48)
        space (char 32)
        string (filter #(not= space %) (seq binary-string))]
  (map #(if (= zero %) 0 1) string)))
(= (binary-string-to-digits "1010") [1 0 1 0])

(defn two-power
  "Computes power(2, n) for n >= 0"
  [n]
  
  (cond
    (< n 0) nil
    (zero? n)
      1
    :else
      (reduce * (repeat n 2))))

(defn binary-to-decimal
  "Converts a binary string into a decimal number."
  [binary-number]
  (let [digits (binary-string-to-digits binary-number)
        exponents (reverse (range (count digits)))
        exponent-digit-pairs (zipmap exponents digits)]
     (reduce + (map #(* (two-power (first %)) (last %)) exponent-digit-pairs))))  

;;;
; Definition of the binary string monad
(defn bs-result
  "Definition of result function of binary string monad"
  [decimal-number]
  (Integer/toBinaryString decimal-number))
  
(defn bs-bind
  "Definition of bind function of binary string monad"
  [mv f]
  (f (binary-to-decimal mv)))

; This monad satisfies the Three Monad Laws.
(defn lift-function-into-monad [function result] (comp result function))
  
; Use the defmonad function in clojure.algo.monads to define a binary string monad
(defmonad binary-string-m
  [m-result bs-result
   m-bind bs-bind])

; Use this monad and the domonad function in clojure.algo.monads to define some binary string functions.
(defn add [x y]
  (domonad binary-string-m
    [a x
     b y]
    (+ a b)))

(defn multiply [x y]
  (domonad binary-string-m
    [a x
     b y]
    (* a b)))

(defn power [x y]
  (with-monad binary-string-m
    (domonad
      [a x
       b y]
      (reduce * (repeat b a)))))
