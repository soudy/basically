(ns basically.expr)

(def ^:dynamic *true* -1)
(def ^:dynamic *false* 0)

(def ^:dynamic *type-mismatch-msg* "?TYPE MISMATCH ERROR")

(defn- expect-types
  "Expect an expression to be true, otherwise throw a type mismatch error. Used
  to make sure an operator received valid operand types."
  [valid-types?]
  (when-not valid-types?
    (throw (Exception. *type-mismatch-msg*))))

(defn- apply-op [op & args]
  (apply @(resolve (symbol (name op))) args))

(defn exec-expr [operator lhs rhs]
  (case operator
    := (do
         (expect-types (or (and (number? lhs) (number? rhs))
                           (and (string? lhs) (string? rhs))))
         (if (= lhs rhs) *true* *false*))
    :<> (do
          (expect-types (or (and (number? lhs) (number? rhs))
                            (and (string? lhs) (string? rhs))))
          (if-not (= lhs rhs) *true* *false*))
    :or (do
          (expect-types (and (number? lhs) (number? rhs)))
          (bit-or (int lhs) (int rhs)))
    :and (do
           (expect-types (and (number? lhs) (number? rhs)))
           (bit-and (int lhs) (int rhs)))
    :+ (cond
         (and (number? lhs) (number? rhs)) (+ lhs rhs)
         (and (string? lhs) (string? rhs)) (str lhs rhs)
         :else (throw (Exception. *type-mismatch-msg*)))
    (:- :* :/) (do
                 (expect-types (and (number? lhs) (number? rhs)))
                 (apply-op operator lhs rhs))
    (:< :<= :> :>=) (cond
                      (and (number? lhs) (number? rhs))
                        (if (apply-op operator lhs rhs) *true* *false*)
                      (and (string? lhs) (string? rhs))
                         ;; Compare the ASCII value of the first character
                         ;; when comparing strings
                         (if (apply-op operator
                                            (int (first lhs))
                                            (int (first rhs)))
                           *true* *false*)
                      :else (throw (Exception. *type-mismatch-msg*)))))
