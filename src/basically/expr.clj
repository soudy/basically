(ns basically.expr
  (:require [basically.constants :refer :all]
            [basically.errors :refer [error-with-mem]]))

(defn- expect-types
  "Expect an expression to be true, otherwise throw a type mismatch error. Used
  to make sure an operator received valid operand types."
  [mem valid-types?]
  (when-not valid-types?
    (error-with-mem :type-mismatch mem)))

(defn- apply-op [op & args]
  (apply @(resolve (symbol (name op))) args))

(defn exec-expr [operator lhs rhs mem]
  (let [expect-types (partial expect-types mem)]
    (case operator
      := (do
           (expect-types (or (and (number? lhs) (number? rhs))
                             (and (string? lhs) (string? rhs))))
           (if (== lhs rhs) basic-true basic-false))
      :<> (do
            (expect-types (or (and (number? lhs) (number? rhs))
                              (and (string? lhs) (string? rhs))))
            (if-not (= lhs rhs) basic-true basic-false))
      :or (do
            (expect-types (and (number? lhs) (number? rhs)))
            (bit-or (int lhs) (int rhs)))
      :and (do
             (expect-types (and (number? lhs) (number? rhs)))
             (bit-and (int lhs) (int rhs)))
      :not (do
             (expect-types (number? rhs))
             (bit-not (int rhs)))
      :+ (cond
           (and (number? lhs) (number? rhs)) (+ lhs rhs)
           (and (string? lhs) (string? rhs)) (str lhs rhs)
           :else (error-with-mem :type-mismatch mem))
      (:- :*) (do
                (expect-types (and (number? lhs) (number? rhs)))
                (apply-op operator lhs rhs))
      :/ (do
           (expect-types (and (number? lhs) (number? rhs)))
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
                        (and (number? lhs) (number? rhs))
                        (if (apply-op operator lhs rhs) basic-true basic-false)

                        ;; Compare the ASCII value of the first character
                        ;; when comparing strings
                        (and (string? lhs) (string? rhs))
                        (if (apply-op operator (int (first lhs)) (int (first rhs)))
                          basic-true
                          basic-false)

                        :else (error-with-mem :type-mismatch mem)))))
