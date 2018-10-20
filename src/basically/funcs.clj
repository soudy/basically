(ns basically.funcs
  (:require [basically.errors :refer [error]]
            [clojure.math.numeric-tower :refer :all]))

(def functions
  {"abs" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (abs n)))

   "asc" (fn [s]
           (if (not (string? s))
             (error :type-mismatch)
             (int (first s))))

   "chr$" (fn [n]
            (if (not (number? n))
              (error :type-mismatch)
              (str (char n))))

   "cos" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (Math/cos n)))

   "exp" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (expt Math/E n)))

   "int" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (floor n)))

   "left$" (fn [s n]
             (if (or (not (string? s)) (not (number? n)))
               (error :type-mismatch)
               (subs s 0 n)))

   "len" (fn [s]
           (if (not (string? s))
             (error :type-mismatch)
             (count s)))

   "log" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (Math/log n)))

   "mid$" (fn [& args] "") ; TODO: implement MID$


   "rnd" (fn [] (rand))

   "right$" (fn [s n]
              (if (or (not (string? s)) (not (number? n)))
                (error :type-mismatch)
                (apply str (take-last n s))))

   "sgn" (fn [n]
           (cond
             (not (number? n)) (error :type-mismatch)
             (zero? n) 0
             (pos? n) 1
             (neg? n) -1))

   "sin" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (Math/sin n)))

   "spc" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (apply str (repeat n " "))))

   "sqr" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (sqrt n)))

   "str$" (fn [n]
            (if (not (number? n))
              (error :type-mismatch)
              (if (or (zero? n) (pos? n))
                (str " " n)
                (str n))))

   "tan" (fn [n]
           (if (not (number? n))
             (error :type-mismatch)
             (Math/tan n)))})
