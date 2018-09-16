(ns basically.parser)

(defrecord Node [type label value])
(defrecord NodeList [label nodes])

(declare parse-node)

(defn- new-node
  ([type] (->Node type nil nil))
  ([type label] (->Node type label nil))
  ([type label value] (->Node type label value)))

(defn- expect
  "Expect the token on top to be any of `types'."
  [[{:keys [type value] :as current} & _] types]
  (if (some #(= type %) types)
    [current rest]
    (throw (Exception. (str "?UNEXPECTED \"" value "\"")))))

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
  (or (= type :newline)
      (= type :colon)))

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
     (let [[current & rest] tokens]
       ;; Print statements specifics. Extra semicolons get ignored and comma's are
       ;; a tabulator margin.
       (case (:type current)
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
  (let [node (new-node type label)
        [value tokens] (expect-and-parse tokens [:integer])
        tokens (expect-end tokens)]
    [(assoc node :value value) tokens]))

;; TODO: parse expressions
(defn- parse-expr
  [[{:keys [type value]} & rest :as tokens] label]
  (case type
    (:ident :integer ))
  [(new-node type label value) rest])

(defn- parse-node
  ([tokens] (parse-node tokens nil))
  ([[{:keys [type value]} & rest :as tokens] label]
   (case type
     :print (parse-print rest label)
     :comment [(new-node :noop label) rest]
     :string [(new-node :string label value) rest]
     (:goto :gosub) (parse-jump rest label type)
     :return (do
               (expect-end rest)
               [(new-node :return label) rest])
     (:ident :integer :float :+ :-) (parse-expr tokens label)
     :colon (parse-node rest label))))

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
