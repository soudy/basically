(ns basically.parser)

(defrecord NodeList [label nodes])
(defrecord Node [type label value])

(defrecord Expr [operator lhs rhs])
(defrecord FuncCall [name args user-function?])
(defrecord IfStmt [condition body])
(defrecord InputStmt [message variables])
(defrecord DefineFunc [name arg body])

(defn- function-call? [[{current :type} {next :type} & _]]
  (and (= current :ident) (= next :lparen)))

(defn- end-delimiter? [{:keys [type]}]
  (or (= type :newline) (= type :colon)))

(defn- new-node
  ([type] (->Node type nil nil))
  ([type label] (->Node type label nil))
  ([type label value] (->Node type label value)))

(defn- expect
  "Expect the token on top to be any of `types'."
  ([[{:keys [value]} & _ :as tokens] types]
   (expect tokens types (str "?UNEXPECTED \"" value "\"")))
  ([[{:keys [type value] :as current} & rest] types message]
  (if (some #{type} types)
    [current rest]
    (throw (Exception. message)))))

(declare parse-node)

(defn- expect-and-parse
  "Expect the token on top to be any of `types' and parse it."
  ([tokens types]
   (expect tokens types)
   (parse-node tokens))
  ([tokens types message]
   (expect tokens types message)
   (parse-node tokens)))

(defn- expect-end
  "Expect the end of a statement."
  [tokens]
  (when (seq tokens)
    (expect tokens [:newline :colon])))

(defn- parse-print
  "Parse a print statement.

  Syntax:
    PRINT <value> {\",\" | \";\" | <value>}

  Examples:
    PRINT \"Hello, world!\"
    PRINT 2;4
    PRINT A,B,C;
  "
  ([tokens label]
   (parse-print tokens (new-node :print label) []))
  ([[{:keys [type] :as current} & rest :as tokens] node values]
   (if (or (empty? tokens) (end-delimiter? current))
     [(assoc node :value values) tokens]
     ;; Print statements specifics. Semicolons mean no break and commas mean
     ;; a tabulator margin.
     (case type
       :semicolon (parse-print rest node (conj values (new-node :nobreak)))
       :comma (parse-print rest node (conj values (new-node :tab-margin)))
       (let [[value tokens] (parse-node tokens)]
         (parse-print tokens node (conj values value)))))))

(defn- parse-input
  "Parse an input statement.

  Syntax:
    INPUT [<string>] <ident> {\",\" <ident>}

  Examples:
    INPUT \"How many? \"; A%
    INPUT A, B, C
    INPUT \"Enter 2 things please \"; A$, B$"
  ([[{:keys [type value]} & rest :as tokens] label]
   (expect tokens [:string :ident] "?ILLEGAL DIRECT ERROR")
   (if (= type :string)
     (let [[_ tokens] (expect rest [:semicolon])]
       (parse-input tokens label value []))
     (parse-input tokens label nil [])))
  ([[current & _ :as tokens] label print-message variables]
   (if (or (empty? tokens) (end-delimiter? current))
     [(new-node label :input (->InputStmt print-message variables)) tokens]
     (let [[variable [current & rest :as tokens]] (expect-and-parse tokens [:ident])
           new-variables (conj variables variable)]
       (if (= (:type current) :comma)
         (parse-input rest label print-message new-variables)
         (do
           (expect-end tokens)
           [(new-node label :input (->InputStmt print-message new-variables)) tokens]))))))

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

;; Operators with their precedence and associativity
(let [operators {:=  {:prec 0 :assoc :right}
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
                 :exp {:prec 7 :assoc :right}}]
  (defn- operator? [{:keys [type]}] (some (partial = type) (keys operators)))
  (defn- get-prec [{:keys [type]}] (get-in operators [type :prec]))
  (defn- get-assoc [{:keys [type]}] (get-in operators [type :assoc])))

(declare parse-expr-begin)

(defn- parse-function-call-args
  "Parse a function call's arguments.

  Syntax:
    <fn-call-args> ::= <expr> {\",\" <expr>}"
  ([tokens]
   (parse-function-call-args tokens []))
  ([[{:keys [type]} & _ :as tokens] args]
   (if (= type :rparen)
     [args tokens]
     (let [[expr [{:keys [type]} & rest :as tokens]] (parse-node tokens)
           new-args (conj args expr)]
       (if (= type :comma)
         (parse-function-call-args rest new-args)
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
    (or (function-call? tokens) (= type :fn)) (parse-function-call tokens)
    (= type :lparen)
      (let [[expr tokens] (parse-expr-begin rest)
            [_ tokens] (expect tokens [:rparen])]
        [expr tokens])
    (some #{type} [:integer :float :ident :string])
      [(new-node type nil value) rest]
    :else (throw (Exception. "?SYNTAX ERROR"))))

(defn- parse-expr-begin
  "Parse the beginning of an expression.

  Syntax:
    <expr> ::= <expr-value> {<operator> <expr-value>}"
  ([tokens]
   (parse-expr-begin tokens 0))
  ([tokens prec]
   (let [[expr tokens] (parse-expr-value tokens)]
     (parse-expr-begin tokens prec expr)))
  ([[current & rest :as tokens] operator-prec expr]
   (if-let [current-prec (get-prec current)]
     (if-not (>= current-prec operator-prec)
       [expr tokens]
       (let [new-prec (case (get-assoc current)
                        :right current-prec
                        :left (inc current-prec))
             [rhs tokens] (parse-expr-begin rest new-prec)
             expr (->Expr (:type current) expr rhs)]
         (parse-expr-begin tokens operator-prec expr)))
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
    DEF FN <ident>\"(\" <integer | float> \")\" = <expression>"
  [tokens label]
  (let [[_ tokens] (expect tokens [:fn])
        [{name :value} tokens] (expect tokens [:ident])
        [_ tokens] (expect tokens [:lparen])
        [arg tokens] (expect-and-parse tokens [:ident])
        [_ tokens] (expect tokens [:rparen])
        [_ tokens] (expect tokens [:=])
        [body tokens] (parse-expr tokens)]
    (expect-end tokens)
    [(new-node :def label (->DefineFunc name arg body))]))

(defn- parse-node
  ([tokens]
   (parse-node tokens nil))
  ([[{:keys [type value]} & rest :as tokens] label]
   (case type
     :print (parse-print rest label)
     :input (parse-input rest label)
     :comment [(new-node :noop label) rest]
     (:goto :gosub) (parse-jump rest label type)
     (:return :new :clr :stop) ; Statements without arguments
      (do
        (expect-end rest)
        [(new-node type label) rest])
     (:ident :float :integer :string)
       (if (or (function-call? tokens) (operator? (first rest)))
         (parse-expr tokens label)
         [(new-node type label value) rest])
     :if (parse-if rest label)
     :def (parse-def rest label)
     :colon (parse-node rest label)
     (throw (Exception. (str "?SYNTAX ERROR" (when label (str " IN " label))))))))

(defn- parse-line ([[{:keys [type value]} & rest :as tokens]]
   ;; If the first token of the line is an integer, set the label to that
   ;; number.
   (if (= type :integer)
     (parse-line rest [] value)
     (parse-line tokens [] nil)))
  ([[{:keys [type]} & rest :as tokens] nodes label]
   (if (or (empty? tokens) (= type :newline))
     (if (= (count nodes) 1)
       [(nth nodes 0) rest]
       [(->NodeList label nodes) rest])
     (let [[node tokens] (parse-node tokens label)]
       (parse-line tokens (conj nodes node) label)))))

(defn parse
  ([tokens]
   (parse tokens []))
  ([tokens ast]
   (if (empty? tokens)
     ast
     (let [[node tokens] (parse-line tokens)]
       (if-not (nil? node)
         (parse tokens (conj ast node))
         (parse tokens ast))))))
