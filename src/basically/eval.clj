(ns basically.eval
  (:require [basically.expr :refer [exec-expr]]
            [basically.lexer :refer [lex]]
            [basically.parser :refer [parse]]
            [basically.mem :as mem]
            [basically.errors :refer [error]]
            [basically.constants :refer [basic-true basic-false]])
  (:import [basically.parser Node Expr FuncCall])
  (:refer-clojure :exclude [eval]))

(declare eval-expr)

(def ^:private func-not-found-value 0)

(defn- eval-func-call [{:keys [name args]} mem]
  (if-let [func (mem/get-func mem name)]
    (apply func (map #(eval-expr % mem) args))
    func-not-found-value))

(defn- eval-expr [expr mem]
  (cond
    ;; Literals
    (instance? Node expr)
    (let [{:keys [type value]} expr]
      (case type
        :expr (eval-expr value mem) ; Expression wrapped in a node
        :ident (mem/get-var mem value)
        (:integer :float :string) value))

    ;; Function call
    (instance? FuncCall expr)
    (eval-func-call expr mem)

    ;; Operations
    (instance? Expr expr)
    (let [{:keys [operator lhs rhs]} expr]
      (if (nil? lhs)
        ;; Unary operator
        (let [rhs (eval-expr rhs mem)]
          (exec-expr operator nil rhs))
        (let [lhs (eval-expr lhs mem)
              rhs (eval-expr rhs mem)]
          (exec-expr operator lhs rhs))))

    :else (error :syntax-error)))

(defn- whole-number? [n]
  (not (> n (int n))))

(defn- eval-print-arg [[current & [next] :as args] mem]
  ;; Print a newline if we're at the last argument, and it's not a semicolon
  (let [print-newline? (and (= (count args) 1) (not= current :nobreak))
        value (cond
                (number? current)
                (let [number (if (whole-number? current) (int current) current)]
                  (cond
                    (string? next) (str number " ")
                    (= next :nobreak) (str number "  ")
                    :else number))

                (string? current)
                (if (number? next)
                  (str current " ")
                  current)

                (= current :tab-margin) (apply str (repeat 10 " "))
                (= current :nobreak) "")]
    (str value (when print-newline? "\n"))))

(defn- eval-print-args
  "Evaluate the argument given to the print statement, forming a single string
  to print."
  ([args mem]
   ;; First evaluate all arguments, then go over the evaluated arguments and
   ;; format them correctly
   (let [args (map (fn [{:keys [type] :as arg}]
                     (case type
                       (:tab-margin :nobreak) type
                       (eval-expr arg mem))) args)
         ;; If the arguments start with a float or integer, indent by a space
         message (if (number? (first args)) " " "")]
     (eval-print-args args mem message)))
  ([args mem message]
   (if (empty? args)
     message
     (let [arg-value (eval-print-arg args mem)]
       (recur (drop 1 args) mem (str message arg-value))))))

(defn- eval-print [{:keys [value]} mem]
  (let [message (eval-print-args value mem)]
    (print message)))

(defn- eval-let [{name :name expr-node :value} mem]
  (mem/set-var! mem name (eval-expr (:value expr-node) mem)))

(defn- assignment-expr? [expr]
  (and (instance? Expr expr)) (= (:operator expr) :=))

(defn- eval-top-level-expr [{:keys [value label]} mem]
  (if (assignment-expr? value)
    (let [name (-> value :lhs :value)
          value (eval-expr (:rhs value) mem)]
      (mem/set-var! mem name value))
    (error :syntax-error label)))

(defn- get-user-input [prompt]
  (print prompt)
  (flush)
  (let [input (read-line)]
    (if (re-matches (re-pattern "(\\d+(\\.\\d+)?)") input)
      (read-string input)
      input)))

(defn- eval-input [{message :message [{variable-name :value} & rest] :variables :as input-stmt} mem]
  (let [prompt (str message "? ")
        input (get-user-input prompt)]
    (mem/set-var! mem variable-name input)
    (when (seq rest)
      (recur (assoc input-stmt :message "?" :variables rest) mem))))

(declare eval-node)

(defn- eval-if [{:keys [condition body]} mem]
  (when (= (eval-expr condition mem) basic-true)
    (if (and (instance? Node body) (= (:type body) :integer))
      ;; A single integer node as if body acts as goto
      (mem/set-jump! mem (:value body))
      (eval-node body mem))))

(declare eval)

(defn run-program [program mem]
  (-> program
      lex
      parse
      (eval mem)))

(defn- eval-run [start-at mem]
  (when-not (nil? start-at)
    (mem/set-jump! mem start-at))
  (run-program (mem/get-program mem) mem))

(defn- eval-gosub [{:keys [value]} label mem]
  (mem/set-jump! mem value))

(defn- eval-for [{:keys [counter to counter-value] :as for-loop} label mem]
  (let [label (if (nil? label) :direct label)
        to-value (eval-expr to mem)
        for-loop (assoc for-loop :label label :to to-value)]
    (when-not (mem/in-loop-stack? mem for-loop)
      (mem/push-loop-stack! mem for-loop)
      (mem/set-var! mem counter counter-value))))

(defn- eval-next [value mem]
  (if-let [{:keys [counter to step label]} (mem/peek-loop-stack mem)]
    (do
      ;; Increment loop counter
      (mem/set-var! mem counter (+ (mem/get-var mem counter) step))
      (if (> (mem/get-var mem counter) to)
        (mem/pop-loop-stack! mem)
        (mem/set-jump! mem label)))
    (error :next-without-for)))

(defn- eval-end [mem]
  (mem/clear! mem)
  (mem/set-end! mem))

(defn- clear-screen []
  (print (str (char 27) "[2J"))
  (print (str (char 27) "[;H")))

(defn- eval-node [{:keys [type value label] :as node} mem]
  (case type
    :print (eval-print node mem)
    :let (eval-let value mem)
    :expr (eval-top-level-expr node mem)
    :input (eval-input value mem)
    :if (eval-if value mem)
    :new (mem/clear! mem)
    :run (eval-run value mem)
    :goto (mem/set-jump! mem (:value value))
    :gosub (eval-gosub value label mem)
    :for (eval-for value label mem)
    :next (eval-next value mem)
    :noop nil
    :end (eval-end mem)
    :clr (clear-screen)
    (error :syntax-error label)))

(defn- find-line-index [ast line]
  (if (= line :direct)
    0 ; One-line for loop in direct mode
    (let [index (keep-indexed #(when (= line (:label %2)) %1) ast)]
      (if-not (seq index)
        nil
        (nth index 0)))))

(defn eval
  "Evaluate an AST."
  ([ast]
   (eval ast (mem/init) 0))
  ([ast mem]
   (eval ast mem 0))
  ([ast mem current]
   (cond
     (mem/get-jump mem)
     (if-let [index (find-line-index ast (mem/get-jump mem))]
       (do
         (mem/clear-jump! mem)
         (recur ast mem index))
       (error :undefd-statement))

     (mem/end? mem)
     (do
       (mem/clear-end! mem)
       mem)

     :else
     (if (= (count ast) current)
       mem
       (let [current-node (get ast current)]
         (eval-node current-node mem)
         (recur ast mem (inc current)))))))
