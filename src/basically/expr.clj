(ns basically.expr
  (:require [basically.errors :refer [error-with-mem]]))

(def basic-true -1)
(def basic-false 0)

(defn- expect-types
  "Expect an expression to be true, otherwise throw a type mismatch error. Used
  to make sure an operator received valid operand types."
  [mem valid-types?]
  (when-not valid-types?
    (error-with-mem :type-mismatch mem)))

(defn- apply-op [op & args]
  (apply @(resolve (symbol (name op))) args))

(defn- strings? [& args]
  (every? string? args))

(defn- numbers? [& args]
  (every? number? args))

(defn exec-expr [operator lhs rhs mem]
  (let [expect-types (partial expect-types mem)]
    (case operator
      (:<> :=) (let [result (cond
                              (numbers? lhs rhs)
                              (if (== lhs rhs) basic-true basic-false)

                              (strings? lhs rhs)
                              (if (= lhs rhs) basic-true basic-false)

                              :else (error-with-mem :type-mismatch mem))]
                 ;; Bitwise NOT outcome if we're doing inequality check instead
                 ;; of equality.
                 (if (= operator :<>)
                   (bit-not result)
                   result))
      :or (do
            (expect-types (numbers? lhs rhs))
            (bit-or (int lhs) (int rhs)))
      :and (do
             (expect-types (numbers? lhs rhs))
             (bit-and (int lhs) (int rhs)))
      :not (do
             (expect-types (number? rhs))
             (bit-not (int rhs)))
      :+ (cond
           (numbers? lhs rhs) (+ lhs rhs)
           (strings? lhs rhs) (str lhs rhs)
           :else (error-with-mem :type-mismatch mem))
      (:- :*) (do
                (expect-types (numbers? lhs rhs))
                (apply-op operator lhs rhs))
      :/ (do
           (when (== rhs 0)
             (error-with-mem :division-by-zero mem))
           (expect-types (numbers? lhs rhs))
           (let [result (/ lhs rhs)]
             (if-not (integer? result)
               (float result)
               result)))
      :unary- (do
                (expect-types (number? rhs))
                (- rhs))
      :unary+ (do
                (expect-types (number? rhs))
                (+ rhs))
      (:< :<= :> :>=) (cond
                        (numbers? lhs rhs)
                        (if (apply-op operator lhs rhs) basic-true basic-false)

                        ;; Compare the ASCII value of the first character
                        ;; when comparing strings
                        (strings? lhs rhs)
                        (if (apply-op operator (int (first lhs)) (int (first rhs)))
                          basic-true
                          basic-false)

                        :else (error-with-mem :type-mismatch mem)))))
