(ns basically.eval
  (:require [basically.expr :refer [exec-expr]]
            [basically.lexer :refer [lex]]
            [basically.parser :refer [parse]]
            [basically.env :as env]
            [basically.errors :refer [error-with-env]])
  (:import [basically.parser Node Expr FuncCall]
           [clojure.lang ArityException])
  (:refer-clojure :exclude [eval]))

(declare eval-expr eval-node eval)

(defn- eval-user-func-call [name args env]
  (when-not (= (count args) 1)
    (error-with-env :syntax-error env))
  (if-let [{:keys [param body]} (env/get-func env name)]
    (let [function-env (atom @env)
          arg (eval-expr (first args) env)]
      (env/set-var! function-env param arg)
      (eval-expr body function-env))
    (error-with-env :undefd-function env)))

(defmulti ^:private eval-expr (fn [expr env] (class expr)))

;;; Literal expression
(defmethod eval-expr Node [{:keys [type value]} env]
  (case type
    :expr (eval-expr value env) ; Expression wrapped in a node
    :ident (env/get-var env value)
    (:integer :float :string) value))

(def ^:private func-not-found-value 0)

;;; Function call expression
(defmethod eval-expr FuncCall [{:keys [name args user-function?]} env]
  (if user-function?
    (eval-user-func-call name args env)
    (let [func (env/get-func env name)]
      (if (fn? func)
        (try
          (apply func (map #(eval-expr % env) args))
          (catch ArityException _
            (error-with-env :syntax-error env)))
        func-not-found-value))))

;;; Arithmetic expression
(defmethod eval-expr Expr [{:keys [operator lhs rhs]} env]
  (if (nil? lhs)
    ;; Unary operator
    (let [rhs (eval-expr rhs env)]
      (exec-expr operator nil rhs env))
    (let [lhs (eval-expr lhs env)
          rhs (eval-expr rhs env)]
      (exec-expr operator lhs rhs env))))

(defmethod eval-expr :default [_ env]
  (error-with-env :syntax-error env))

(defn- whole-number? [n]
  (== n (int n)))

(defn- format-print-arg [[current & [next] :as args]]
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
  ([args env]
   ;; First evaluate all arguments, then go over the evaluated arguments and
   ;; format them correctly
   (let [args (map (fn [{:keys [type] :as arg}]
                     (case type
                       (:tab-margin :nobreak) type
                       (eval-expr arg env))) args)
         ;; If the arguments start with a float or integer, indent by a space
         message (if (number? (first args)) " " "")]
     (eval-print-args args env message)))
  ([args env message]
   (if (empty? args)
     message
     (let [arg-value (format-print-arg args)]
       (recur (rest args) env (str message arg-value))))))

(defn- eval-print [args env]
  (let [message (eval-print-args args env)]
    (print message)))

(defn- eval-let [{name :name expr :value} env]
  (env/set-var! env name (eval-expr expr env)))

(defn- assignment-expr? [expr]
  (and (instance? Expr expr)) (= (:operator expr) :=))

(defn- eval-top-level-expr [expr env]
  (if (assignment-expr? expr)
    (let [name (-> expr :lhs :value)
          value (eval-expr (:rhs expr) env)]
      (env/set-var! env name value))
    (error-with-env :syntax-error env)))

(defn- get-user-input [prompt]
  (print prompt)
  (flush)
  (let [input (read-line)]
    (if (re-matches #"(\d+(\.\d+)?)" input)
      (read-string input)
      input)))

(defn- eval-input [{message :message [{variable-name :value} & rest] :variables :as input-stmt} env]
  (let [prompt (str message "? ")
        input (get-user-input prompt)]
    (env/set-var! env variable-name input)
    (when (seq rest)
      (recur (assoc input-stmt :message "?" :variables rest) env))))

(defn- stop-evaluating?
  "Should we stop evaluating a statement body? This is true if control flow
  statements like GOTO or END are used in a statement body."
  [env]
  (or (env/get-jump env) (env/end? env)))

(defn- eval-if [{:keys [condition body]} env]
  (when-not (= (eval-expr condition env) 0)
    (loop [[node & rest :as nodes] body]
      ;; Evaluate next node in if body if there is one and there is no jump or
      ;; end set.
      (when (and nodes (not (stop-evaluating? env)))
        (eval-node node env)
        (recur rest)))))

(defn run-program
  ([program]
   (-> program
       lex
       parse
       eval))
  ([program env]
   (-> program
       lex
       parse
       (eval env))))

(defn run-file [filename]
  (try
    (run-program (slurp filename))
    (catch Exception e
      (println (str "Failed reading file: " (.getMessage e)))
      (System/exit 1))))

(defn- eval-run [start-at env]
  (when-not (nil? start-at)
    (env/set-jump! env start-at))
  (run-program (env/get-program env) env))

(defn- eval-gosub [{:keys [value]} env]
  (env/set-jump! env value))

(defn- eval-for [{:keys [counter to counter-value] :as for-loop} env]
  (let [for-loop (-> for-loop
                     (assoc :label (get-in @env [:current-label] :direct))
                     (assoc :to (eval-expr to env)))]
    (when-not (env/in-loop-stack? env for-loop)
      (env/push-loop-stack! env for-loop)
      (env/set-var! env counter counter-value))))

(defn- eval-next [value env]
  (if-let [{:keys [counter to step label]} (env/peek-loop-stack env)]
    (do
      ;; Increment loop counter
      (env/set-var! env counter (+ (env/get-var env counter) step))
      (if (> (env/get-var env counter) to)
        (env/pop-loop-stack! env)
        (env/set-jump! env label)))
    (error-with-env :next-without-for env)))

(defn- eval-end [env]
  (env/clear! env)
  (env/set-end! env))

(defn- clear-screen []
  (print (str (char 27) "[2J"))
  (print (str (char 27) "[;H")))

(defn- eval-def [def-node env]
  (env/define-function! env def-node))

(defn- eval-node [{:keys [type value]} env]
  (case type
    :print (eval-print value env)
    :let (eval-let value env)
    :expr (eval-top-level-expr value env)
    :input (eval-input value env)
    :if (eval-if value env)
    :new (env/clear! env)
    :run (eval-run value env)
    :goto (env/set-jump! env (:value value))
    :gosub (eval-gosub value env)
    :for (eval-for value env)
    :next (eval-next value env)
    :noop nil
    :end (eval-end env)
    :clr (clear-screen)
    :def (eval-def value env)
    (error-with-env :syntax-error env)))

(defn- find-line-index [ast line]
  (if (= line :direct)
    0 ; One-line for loop in direct mode
    (let [index (keep-indexed #(when (= line (:label %2)) %1) ast)]
      (if-not (seq index)
        nil
        (first index)))))

(defn eval
  "Evaluate an AST."
  ([ast]
   (eval ast (env/init)))
  ([ast env]
   (loop [position 0]
     (cond
       (env/get-jump env)
       (if-let [index (find-line-index ast (env/get-jump env))]
         (do
           (env/clear-jump! env)
           (recur index))
         (error-with-env :undefd-statement env))

       (env/end? env)
       (do
         (env/clear-end! env)
         env)

       :else
       (if (= (count ast) position)
         env
         (let [current-node (get ast position)]
           (env/set-current-label! env (:label current-node))
           (eval-node current-node env)
           (recur (inc position))))))))
