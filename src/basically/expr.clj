(ns basically.expr)

(def basic-true -1)
(def basic-false 0)

(def type-mismatch-err "?TYPE MISMATCH ERROR")

(defn- expect-types
  "Expect an expression to be true, otherwise throw a type mismatch error. Used
  to make sure an operator received valid operand types."
  [valid-types?]
  (when-not valid-types?
    (throw (Exception. type-mismatch-err))))

(defn- apply-op [op & args]
  (apply @(resolve (symbol (name op))) args))

(defn exec-expr [operator lhs rhs]
  (case operator
    := (do
         (expect-types (or (and (number? lhs) (number? rhs))
                           (and (string? lhs) (string? rhs))))
         (if (= lhs rhs) basic-true basic-false))
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
    :+ (cond
         (and (number? lhs) (number? rhs)) (+ lhs rhs)
         (and (string? lhs) (string? rhs)) (str lhs rhs)
         :else (throw (Exception. type-mismatch-err)))
    (:- :* :/) (do
                 (expect-types (and (number? lhs) (number? rhs)))
                 (apply-op operator lhs rhs))
    (:< :<= :> :>=) (cond
                      (and (number? lhs) (number? rhs))
                        (if (apply-op operator lhs rhs) basic-true basic-false)
                      (and (string? lhs) (string? rhs))
                         ;; Compare the ASCII value of the first character
                         ;; when comparing strings
                         (if (apply-op operator
                                            (int (first lhs))
                                            (int (first rhs)))
                           basic-true basic-false)
                      :else (throw (Exception. type-mismatch-err)))))
