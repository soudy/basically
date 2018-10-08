(ns basically.parser
  (:require [basically.errors :refer [error]]))

(defrecord Node [type label value])

(defrecord Expr [operator lhs rhs])
(defrecord FuncCall [name args user-function?])
(defrecord IfStmt [condition body])
(defrecord InputStmt [message variables])
(defrecord DefineFunc [name arg body])
(defrecord LetStmt [name value])
(defrecord ForLoop [counter counter-value to step])

(defn- function-call? [[{current :type} {next :type}]]
  (or (= current :fn)
      (and (= current :ident) (= next :lparen))))

(defn- end-delimiter? [[{:keys [type]}]]
  (or (= type :newline) (= type :colon)))

(defn- new-node
  ([type] (->Node type nil nil))
  ([type label] (->Node type label nil))
  ([type label value] (->Node type label value)))

(defn- expect
  "Expect the token on top to be any of `types'."
  [[{:keys [type value] :as current} & rest] types]
  (if (some #{type} types)
    [current rest]
    (error :syntax-error)))

(declare parse-node)

(defn- expect-and-parse
  "Expect the token on top to be any of `types' and parse it."
  [tokens types]
  (expect tokens types)
  (parse-node tokens))

(defn- expect-end
  "Expect the end of a statement."
  [tokens]
  (when (seq tokens)
    (expect tokens [:newline :colon])))

(defn- parse-print
  "Parse a print statement.

  Syntax:
    PRINT <expr> {\",\" | \";\" | <expr>}

  Examples:
    PRINT \"Hello, world!\"
    PRINT 2;4
    PRINT A,B,C;"
  ([tokens label]
   (parse-print tokens (new-node :print label) []))
  ([[{:keys [type]} & rest :as tokens] node values]
   (if (or (empty? tokens) (end-delimiter? tokens))
     [(assoc node :value values) tokens]
     ;; Print statements specifics. Semicolons mean no break and commas mean
     ;; a tabulator margin.
     (case type
       :semicolon (recur rest node (conj values (new-node :nobreak)))
       :comma (recur rest node (conj values (new-node :tab-margin)))
       (let [[value tokens] (parse-node tokens)]
         (recur tokens node (conj values value)))))))

(defn- parse-input
  "Parse an input statement.

  Syntax:
    INPUT [<string> \";\"] <ident> {\",\" <ident>}

  Examples:
    INPUT \"How many? \"; A%
    INPUT A, B, C
    INPUT \"Enter 2 things please \"; A$, B$"
  ([[{:keys [type value]} & rest :as tokens] label]
   (expect tokens [:string :ident])
   (if (= type :string)
     (let [[_ tokens] (expect rest [:semicolon])]
       (parse-input tokens label value []))
     (parse-input tokens label nil [])))
  ([tokens label print-message variables]
   (if (or (empty? tokens) (end-delimiter? tokens))
     [(new-node :input label (->InputStmt print-message variables)) tokens]
     (let [[variable [current & rest :as tokens]] (expect-and-parse tokens [:ident])
           new-variables (conj variables variable)]
       (if (= (:type current) :comma)
         (recur rest label print-message new-variables)
         (do
           (expect-end tokens)
           [(new-node :input label (->InputStmt print-message new-variables)) tokens]))))))

(defn- parse-jump
  "Parse the GOTO and GOSUB statements.

  Syntax:
    GOTO <integer> | GOSUB <integer>

  Examples:
    GOTO 10
    GOSUB 20"
  [tokens label type]
  (let [[arg tokens] (expect-and-parse tokens [:integer])]
    (expect-end tokens)
    [(new-node type label arg) tokens]))

(defn- parse-run
  "Parse a RUN statement.

  Syntax:
    RUN [<integer>]

  Examples:
    RUN
    RUN 50"
  [tokens label]
  (if (or (empty? tokens) (end-delimiter? tokens))
    [(new-node :run label) tokens]
    (let [[{jump :value} tokens] (expect tokens [:integer])]
      [(new-node :run label jump) tokens])))

;; Operators with their precedence and associativity
(let [operators {:= {:prec 0 :assoc :right}
                 :or {:prec 1 :assoc :right}
                 :and {:prec 2 :assoc :right}
                 :< {:prec 3 :assoc :right}
                 :<= {:prec 3 :assoc :right}
                 :> {:prec 3 :assoc :right}
                 :>= {:prec 3 :assoc :right}
                 :<> {:prec 3 :assoc :right}
                 :- {:prec 4 :assoc :left}
                 :+ {:prec 4 :assoc :left}
                 :* {:prec 5 :assoc :left}
                 :/ {:prec 5 :assoc :left}
                 :not {:prec 6 :assoc :left}
                 :unary- {:prec 7 :assoc :left}
                 :unary+ {:prec 7 :assoc :left}}]
  (defn- operator? [type] (some (partial = type) (keys operators)))
  (defn- unary-operator? [type] (some (partial = type) [:not :unary- :unary+]))
  (defn- get-prec [type] (get-in operators [type :prec]))
  (defn- get-assoc [type] (get-in operators [type :assoc])))

(declare parse-expr-begin)

(defn- parse-function-call-args
  "Parse a function call's arguments.

  Syntax:
    <fn-call-args> ::= [<expr>] {\",\" <expr>}"
  ([tokens]
   (parse-function-call-args tokens []))
  ([[{:keys [type]} :as tokens] args]
   (if (= type :rparen)
     [args tokens]
     (let [[expr [{:keys [type]} & rest :as tokens]] (parse-node tokens)
           new-args (conj args expr)]
       (if (= type :comma)
         (recur rest new-args)
         [new-args tokens])))))

(defn- parse-function-call
  "Parse a function call.

  Syntax:
    <fn-call> ::= [FN] <ident> \"(\" <fn-call-args> \")\""
  ([[{:keys [type]} & rest :as tokens]]
   (if (= type :fn)
     (parse-function-call rest true)
     (parse-function-call tokens false)))
  ([[{:keys [value]} & rest] user-function?]
   (let [name value
         [_ tokens] (expect rest [:lparen])
         [args tokens] (parse-function-call-args tokens)
         [_ tokens] (expect tokens [:rparen])]
     [(->FuncCall name args user-function?) tokens])))

(defn- parse-expr-value
  "Parse an expression value.

  Syntax:
    <expr-value> ::= <unary-op> <expr> | \"(\" <expr> \")\" | <value> | <fn-call>"
  [[{:keys [type value]} & rest :as tokens]]
  (cond
    (unary-operator? type)
    (let [prec (get-prec type)
          [expr tokens] (parse-expr-begin rest prec)]
      [(->Expr type nil expr) tokens])

    (function-call? tokens)
    (parse-function-call tokens)

    (= type :lparen)
    (let [[expr tokens] (parse-expr-begin rest)
          [_ tokens] (expect tokens [:rparen])]
      [expr tokens])

    (some #{type} [:integer :float :ident :string])
    [(new-node type nil value) rest]

    :else (error :syntax-error)))

(defn- parse-expr-begin
  "Parse the beginning of an expression.

  Syntax:
    <expr> ::= <expr-value> {<operator> <expr-value>}"
  ([tokens]
   (parse-expr-begin tokens 0))
  ([tokens prec]
   (let [[expr tokens] (parse-expr-value tokens)]
     (parse-expr-begin tokens prec expr)))
  ([[{:keys [type]} & rest :as tokens] operator-prec expr]
   (if-let [current-prec (get-prec type)]
     (if-not (>= current-prec operator-prec)
       [expr tokens]
       (let [new-prec (case (get-assoc type)
                        :right current-prec
                        :left (inc current-prec))
             [rhs tokens] (parse-expr-begin rest new-prec)
             expr (->Expr type expr rhs)]
         (recur tokens operator-prec expr)))
     [expr tokens])))

(defn- parse-expr
  ([tokens]
   (parse-expr tokens nil))
  ([tokens label]
   (let [[expr tokens] (parse-expr-begin tokens)]
     [(new-node :expr label expr) tokens])))

(defn- parse-if
  "Parse an if statement.

  Syntax:
    IF <expr> THEN <expr> | IF <expr> GOTO <integer>"
  [tokens label]
  (let [[condition [{:keys [type]} & rest :as tokens]] (parse-expr tokens)]
    (expect tokens [:then :goto])
    (let [[body tokens] (if (= type :goto)
                          (parse-jump rest nil :goto)
                          (parse-node rest))]
      (expect-end tokens)
      [(new-node :if label (->IfStmt condition body)) tokens])))

(defn- parse-def
  "Parse a function statement.

  Syntax:
    DEF FN <ident>\"(\" <ident> \")\" = <expression>"
  [tokens label]
  (let [[_ tokens] (expect tokens [:fn])
        [{name :value} tokens] (expect tokens [:ident])
        [_ tokens] (expect tokens [:lparen])
        [{arg :value} tokens] (expect tokens [:ident])
        [_ tokens] (expect tokens [:rparen])
        [_ tokens] (expect tokens [:=])
        [body tokens] (parse-expr tokens)]
    (expect-end tokens)
    [(new-node :def label (->DefineFunc name arg body)) tokens]))

(defn- parse-let
  "Parse a let statement.

  Syntax:
    LET <ident> = <expr>"
  [tokens label]
  (let [[{name :value} tokens] (expect tokens [:ident])
        [_ tokens] (expect tokens [:=])
        [value tokens] (parse-expr tokens)]
    (expect-end tokens)
    [(new-node :let label (->LetStmt name value)) tokens]))

(def ^:private default-for-step 1)

(defn- parse-for
  "Parse a for statement.

  Syntax:
    FOR <ident>=<number> TO <expr> [STEP <number>]"
  [tokens label]
  (let [[{counter :value} tokens] (expect tokens [:ident])
        [_ tokens] (expect tokens [:=])
        [{counter-value :value} tokens] (expect tokens [:integer :float])
        [_ tokens] (expect tokens [:to])
        [to tokens] (parse-expr tokens)]
    (if (or (empty? tokens) (end-delimiter? tokens))
      [(new-node :for label (->ForLoop counter counter-value to default-for-step))
       tokens]
      (let [[_ tokens] (expect tokens [:step])
            [{step :value} tokens] (expect tokens [:integer :float])]
        (expect-end tokens)
        [(new-node :for label (->ForLoop counter counter-value to step))
         tokens]))))

(defn- parse-next
  "Parse a next statement.

  Syntax:
    NEXT [<ident>] {\",\" <ident>}"
  ([tokens label]
   (parse-next tokens label []))
  ([tokens label args]
   (if (or (empty? tokens) (end-delimiter? tokens))
     [(new-node :next label args) tokens]
     (let [[arg [{next :type} & rest :as tokens]] (expect-and-parse tokens [:ident])
           new-args (conj args arg)]
       (if (= next :comma)
         (recur rest label new-args)
         (do
           (expect-end tokens)
           [(new-node :next label new-args) tokens]))))))

(defn- parse-node
  ([tokens]
   (parse-node tokens nil))
  ([[{:keys [type value]} & rest :as tokens] label]
   (case type
     :print (parse-print rest label)
     :input (parse-input rest label)
     :comment [(new-node :noop label) rest]
     (:goto :gosub) (parse-jump rest label type)
     :run (parse-run rest label)
     (:return :new :clr :stop :end) ; Statements without arguments
     (do
       (expect-end rest)
       [(new-node type label) rest])
     (:ident :float :integer :string)
     (if (or (function-call? tokens) (operator? (:type (first rest))))
       (parse-expr tokens label)
       [(new-node type label value) rest])
     :fn (parse-expr tokens label)
     :if (parse-if rest label)
     :def (parse-def rest label)
     :let (parse-let rest label)
     :for (parse-for rest label)
     :next (parse-next rest label)
     :colon (parse-node rest label)
     (if (operator? type)
       (parse-expr tokens label)
       (error :syntax-error label)))))

(defn direct-statement?
  "Determine if an input is a direct statement or not."
  [[{:keys [type]}]]
  (not= type :integer))

(defn- parse-line
  ([[{:keys [value]} & rest :as tokens]]
   (if (direct-statement? tokens)
     (parse-line tokens [] nil)
     (parse-line rest [] value)))
  ([[{:keys [type]} & rest :as tokens] nodes label]
   (if (or (empty? tokens) (= type :newline))
     [nodes rest]
     (let [[node tokens] (parse-node tokens label)]
       (recur tokens (conj nodes node) label)))))

(defn parse
  ([tokens]
   (parse tokens []))
  ([tokens ast]
   (if (empty? tokens)
     (vec (sort-by :label ast))
     (let [[node tokens] (parse-line tokens)]
       (recur tokens (concat ast node))))))
