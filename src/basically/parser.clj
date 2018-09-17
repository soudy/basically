(ns basically.parser)

(defrecord Node [type label value])
(defrecord NodeList [label nodes])
(defrecord Expr [operator lhs rhs])

;; Operators with their precedence and associativity
(let [operators {:=  {:prec 0 :assoc :right}
                 :< {:prec 1 :assoc :right}
                 :<= {:prec 1 :assoc :right}
                 :> {:prec 1 :assoc :right}
                 :>= {:prec 1 :assoc :right}
                 :<> {:prec 1 :assoc :right}
                 :or {:prec 2 :assoc :right}
                 :and {:prec 3 :assoc :right}
                 :- {:prec 4 :assoc :left}
                 :+ {:prec 4 :assoc :left}
                 :* {:prec 5 :assoc :left}
                 :/ {:prec 5 :assoc :left}
                 :not {:prec 6 :assoc :left}
                 :exp {:prec 7 :assoc :right}}]
  (defn- is-operator? [{:keys [type]}] (some #(= type %) (keys operators)))
  (defn- get-prec [{:keys [type]}] (get-in operators [type :prec]))
  (defn- get-assoc [{:keys [type]}] (get-in operators [type :assoc])))

(defn- is-function-call? [[current next & _]]
  (and (= (:type current) :ident) (= (:type next) :lparen)))

(defn- new-node
  ([type] (->Node type nil nil))
  ([type label] (->Node type label nil))
  ([type label value] (->Node type label value)))

(defn- expect
  "Expect the token on top to be any of `types'."
  [[{:keys [type value] :as current} & rest] types]
  (if (some #(= type %) types)
    [current rest]
    (throw (Exception. (str "?UNEXPECTED \"" value "\"")))))

(declare parse-node)

(defn- expect-and-parse
  "Expect the token on top to be any of `types' and parse it."
  [tokens types]
  (expect tokens types)
  (parse-node tokens))

(defn- expect-end
  "Expect the end of a statement."
  [tokens]
  (when-not (empty? tokens)
    (expect tokens [:newline :colon])
    tokens))

(defn- is-end-delimiter? [{:keys [type]}]
  (or (= type :newline) (= type :colon)))

(defn- parse-print
  "Parse a print statement.

  Syntax:
    PRINT <value> [<, | ;><value>...]

  Examples:
    PRINT \"Hello, world!\"
    PRINT 2;4
    PRINT A,B,C;
  "
  ([tokens label] (parse-print tokens (new-node :print label) []))
  ([tokens node values]
   (if (or (empty? tokens) (is-end-delimiter? (first tokens)))
     [(assoc node :value values) tokens]
     (let [[{:keys [type]} & rest] tokens]
       ;; Print statements specifics. Semicolons mean no break and commas mean
       ;; a tabulator margin.
       (case type
         :semicolon (parse-print rest node (conj values (new-node :nobreak)))
         :comma (parse-print rest node (conj values (new-node :tab-margin)))
         (let [[value tokens] (parse-node tokens)]
           (parse-print tokens node (conj values value))))))))

(defn- parse-jump
  "Parse the GOTO and GOSUB statements.

  Syntax:
    GOTO <integer> | GOSUB <integer>

  Examples:
    GOTO 10
    GOSUB 20
  "
  [tokens label type]
  (let [[arg tokens] (expect-and-parse tokens [:integer])
        tokens (expect-end tokens)]
    [(new-node type label arg) tokens]))

(declare parse-expr-part)

(defn- parse-expr-value [[{:keys [type value]} & rest :as tokens]]
  (cond
    (= type :lparen)
      (let [[expr tokens] (parse-expr-part rest)
            [_ tokens] (expect tokens [:rparen])]
        [expr tokens])
    (some #{type} [:integer :float :ident])
      [(new-node type nil value) rest]
    :else (throw (Exception. "?SYNTAX ERROR"))))

(defn- parse-expr-part
  ([tokens] (parse-expr-part tokens 0))
  ([tokens prec]
   (let [[expr tokens] (parse-expr-value tokens)]
     (parse-expr-part tokens prec expr)))
  ([[current & rest :as tokens] operator-prec expr]
   (if-let [current-prec (get-prec current)]
     (if-not (>= current-prec operator-prec)
       [expr tokens]
       (let [new-prec (case (get-assoc current) :right current-prec :left (inc current-prec))
             [expr2 tokens] (parse-expr-part rest new-prec)
             expr (->Expr (:type current) expr expr2)]
         (parse-expr-part tokens operator-prec expr)))
     [expr tokens])))

(defn- parse-expr
  "Parse an expression.

  <expr>
  TODO: document grammar"
  [tokens label]
  (let [[expr tokens] (parse-expr-part tokens)]
    (expect-end tokens)
    [(new-node :expr label expr) tokens]))

(defn- parse-node
  ([tokens] (parse-node tokens nil))
  ([[{:keys [type value]} & rest :as tokens] label]
   (case type
     :print (parse-print rest label)
     :comment [(new-node :noop label) rest]
     (:goto :gosub) (parse-jump rest label type)
     (:return :new :clr :stop) ; Statements without arguments
      (do
        (expect-end rest)
        [(new-node type label) rest])
     :lparen (parse-expr tokens label)
     :string [(new-node :string label value) rest]
     (:ident :float :integer)
       (if (or (is-function-call? tokens) (is-operator? (first rest)))
         (parse-expr tokens label)
         [(new-node type label value) rest])
     :colon (parse-node rest label)
     (throw (Exception. (str "?SYNTAX ERROR" (when label (str " IN " label))))))))

(defn- parse-line
  ([[{:keys [type value]} & rest :as tokens]]
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
  ([tokens] (parse tokens []))
  ([tokens ast]
   (if (empty? tokens)
     ast
     (let [[node tokens] (parse-line tokens)]
       (if-not (nil? node)
         (parse tokens (conj ast node))
         (parse tokens ast))))))
