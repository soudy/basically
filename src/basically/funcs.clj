(ns basically.funcs
  (:require [basically.expr :refer [type-mismatch-err]]
            [clojure.math.numeric-tower :refer :all]))

(defrecord Function [arity func])

(def functions
  {"abs" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (abs n)))

   "asc" (fn [s]
           (if (not (string? s))
             (throw (IllegalArgumentException. type-mismatch-err))
             (int (first s))))

   "chr$" (fn [n]
            (if (not (number? n))
              (throw (IllegalArgumentException. type-mismatch-err))
              (str (char n))))

   "cos" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (Math/cos n)))

   "exp" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (expt Math/E n)))

   "int" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (floor n)))

   "left$" (fn [s n]
             (if (or (not (string? s)) (not (number? n)))
               (throw (IllegalArgumentException. type-mismatch-err))
               (subs s 0 n)))

   "len" (fn [s]
           (if (not (string? s))
             (throw (IllegalArgumentException. type-mismatch-err))
             (count s)))

   "log" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (Math/log n)))

   "mid$" (fn [& args] "") ; TODO: implement MID$


   "rnd" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (rand)))

   "right$" (fn [s n]
             (if (or (not (string? s)) (not (number? n)))
               (throw (IllegalArgumentException. type-mismatch-err))
               (apply str (take-last n s))))

   "sgn" (fn [n]
           (cond
             (not (number? n)) (throw (IllegalArgumentException. type-mismatch-err))
             (zero? n) 0
             (pos? n) 1
             (neg? n) -1))

   "sin" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (Math/sin n)))

   "spc" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (apply str (repeat n " "))))

   "sqr" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (sqrt n)))

   "str$" (fn [n]
            (if (not (number? n))
              (throw (IllegalArgumentException. type-mismatch-err))
              (if (or (zero? n) (pos? n))
                (str " " n)
                (str n))))

   "tan" (fn [n]
           (if (not (number? n))
             (throw (IllegalArgumentException. type-mismatch-err))
             (Math/tan n)))
   })
