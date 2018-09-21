(ns basically.eval
  (:require [basically.expr :refer [exec-expr]]
            [basically.mem :refer :all]
            [basically.constants :refer [basic-true basic-false]])
  (:import [basically.parser Node NodeList Expr FuncCall])
  (:refer-clojure :exclude [eval]))

(defn- node-number? [type] (or (= type :integer) (= type :float)))

(declare eval-expr)

(defn- eval-func-call [{:keys [name args]} mem]
  (if-let [func (mem-get-func mem name)]
    (apply func (map #(eval-expr % mem) args))
    0))

(defn- eval-expr [expr mem]
  (cond
    ;; Literals
    (instance? Node expr)
    (let [{:keys [type value]} expr]
      (case type
        :expr (eval-expr value mem) ; Expression wrapped in a node
        :ident (mem-get-var mem value)
        (:integer :float) (read-string value)
        :string value))

    ;; Function call
    (instance? FuncCall expr)
    (eval-func-call expr mem)

    ;; Operations
    (instance? Expr expr)
    (let [{:keys [operator lhs rhs]} expr
          lhs (eval-expr lhs mem)
          rhs (eval-expr rhs mem)]
      (exec-expr operator lhs rhs))))

(defn- eval-print-arg [[{:keys [type value]} & [{next-type :type}] :as args] mem]
  ;; Print a newline if we're at the last argument, and it's not a semicolon
  (let [print-newline? (and (= (count args) 1) (not= type :nobreak))
        value (case type
                (:integer :float) (case next-type
                                    :string (str value " ")
                                    :nobreak (str value "  ")
                                    value)
                :string (if (or (node-number? next-type) (= next-type :expr))
                          (str value " ")
                          value)
                :expr (eval-expr value mem)
                :tab-margin (apply str (repeat 10 " "))
                :nobreak "")]
    (str value (when print-newline? "\n"))))

(defn- eval-print-args
  "Evaluate the argument given to the print statement, forming a single string
  to print."
  ([[{:keys [type]} & rest :as args] mem]
   ;; If the arguments start with a float or integer, indent by a space
   (let [begin-message (if (node-number? type) " " "")]
     (eval-print-args args mem begin-message)))
  ([args mem message]
   (if (empty? args)
     message
     (let [arg-value (eval-print-arg args mem)]
       (recur (drop 1 args) mem (str message arg-value))))))

(defn- eval-print [{:keys [value]} mem]
  (let [message (eval-print-args value mem)]
    (print message)))

(defn- eval-let [{name :name expr-node :value} mem]
  (mem-set-var! mem name (eval-expr (:value expr-node) mem)))

(defn- assignment-expr? [expr]
  (and (instance? Expr expr)) (= (:operator expr) :=))

(defn- eval-top-level-expr [{:keys [value label]} mem]
  (if (assignment-expr? value)
    (let [name (-> value :lhs :value)
          value (eval-expr (:rhs value) mem)]
      (mem-set-var! mem name value))
    (throw (Exception. "?SYNTAX ERROR" (when label (str "IN " label))))))

(defn- get-user-input [prompt]
  (print prompt)
  (flush)
  (let [input (read-line)]
    (if (re-matches (re-pattern "(\\d+(\\.\\d+)?)") input)
      (read-string input)
      (input))))

(defn- eval-input [{message :message [{variable-name :value} & rest] :variables :as input-stmt} mem]
  (let [prompt (str message "? ")
        input (get-user-input prompt)]
    (mem-set-var! mem variable-name input)
    (when (seq rest)
      (eval-input (assoc input-stmt :message "?" :variables rest) mem))))

(declare eval-node)

(defn- eval-if [ast {:keys [condition body]} mem]
  (when (= (eval-expr condition mem) basic-true)
    (eval-node ast body mem)))

(defn- eval-node [ast {:keys [type value] :as current} mem]
  (case type
    :print (eval-print current mem)
    :let (eval-let value mem)
    :expr (eval-top-level-expr current mem)
    :input (eval-input value mem)
    :if (eval-if ast value mem)
    :new (mem-reset! mem)))

(defn eval
  "Evaluate an AST."
  ([ast]
   (eval ast (mem-init) 0))
  ([ast mem]
   (eval ast mem 0))
  ([ast mem current]
   (if (= (count ast) current)
     mem
     (let [current-node (get ast current)]
       (if (instance? NodeList current-node)
         (doseq [node (:nodes current-node)]
           (eval-node ast node mem))
         (eval-node ast current-node mem))
       (recur ast mem (inc current))))))
