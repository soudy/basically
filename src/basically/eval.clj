(ns basically.eval
  (:require [basically.expr :refer [exec-expr]]
            [basically.mem :refer :all])
  (:import [basically.parser Node NodeList Expr])
  (:refer-clojure :exclude [eval]))

(defn- node-number? [type] (or (= type :integer) (= type :float)))

(defn- eval-expr [expr mem]
  (cond
    ;; Literals
    (instance? Node expr)
    (let [{:keys [type value]} expr]
      (case type
        :ident (mem-get-var mem value)
        (:integer :float) (read-string value)
        :string value))

    ;; Operations
    (instance? Expr expr)
    (let [{:keys [operator lhs rhs]} expr
          lhs (eval-expr lhs mem)
          rhs (eval-expr rhs mem)]
      (exec-expr operator lhs rhs))))

(defn- eval-print-arg [[{:keys [type value]} & [{next-type :type} & _] :as args] mem]
  ;; Print a newline if we're at the last argument, and it's not a semicolon
  (let [print-newline? (and (= (count args) 1) (not= type :nobreak))
        value (case type
                (:integer :float) (cond
                                    (= next-type :string) (str value " ")
                                    (= next-type :nobreak) (str value "  ")
                                    :else value)
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

(defn- eval-let [{:keys [name value]} mem]
  (mem-set-var! mem name (eval-expr value mem)))

(defn- assignment-expr? [expr]
  (and (instance? Expr expr)) (= (:operator expr) :=))

(defn- eval-top-level-expr [{:keys [value label]} mem]
  (if (assignment-expr? value)
    (let [name (-> value :lhs :value)
          value (eval-expr (:rhs value) mem)]
      (mem-set-var! mem name value))
    (throw (Exception. "?SYNTAX ERROR" (when label (str "IN " label))))))

(defn- eval-node [ast {:keys [type] :as current} mem]
  (if (instance? NodeList current)
    (map #(eval-node ast % mem) current)
    (case type
      :print (eval-print current mem)
      :let (eval-let current mem)
      :expr (eval-top-level-expr current mem))))

(defn eval
  "Evaluate an AST."
  ([ast]
   (eval ast (mem-init) 0))
  ([ast mem current]
   (if (= (count ast) current)
     mem
     (do
       (eval-node ast (get ast current) mem)
       (recur ast mem (inc current))))))
