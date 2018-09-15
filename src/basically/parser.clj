(ns basically.parser)

(defrecord Node [type label value opts])
(defrecord NodeList [label nodes])

(declare parse-node)

(defn- expect
  "Expect the token on top to be any of `types'."
  [tokens types]
  (let [[{current-type :type current-value :value} current & _] tokens]
    (if (some #(= current-type %) types)
      [current rest]
      (throw (Exception. (str "?UNEXPECTED \"" current-value "\""))))))

(defn- expect-and-parse
  "Expect the token on top to be any of `types' and parse it."
  [tokens types]
  (try
    (do
      (expect tokens types)
      (parse-node tokens))
    (catch Exception e (throw e))))

(defn- expect-end
  "Expect the end of a statement."
  [tokens]
  (try
    (when (not (empty? tokens))
      (do
        (expect tokens [:newline :colon])
        tokens))
    (catch Exception e (throw e))))

(defn- is-end-delimiter? [{:keys [type]}]
  (or (= type :newline)
      (= type :colon)))

(defn- print-newline?
  "Determine if we print a newline or not. If a PRINT statement ends with a `;',
  don't print a newline."
  [tokens]
  (let [args (take-while #(not (is-end-delimiter? %)) tokens)]
    (not= (:type (last args)) :semicolon)))

(defn- parse-print
  "Parse a print statement.

  Examples:
    PRINT \"Hello, world!\"
    PRINT 20
    PRINT A,B,C;
  "
  ([tokens label]
   ;; Determine if we print a newline
   (let [print-newline? (print-newline? tokens)
         node (->Node :print label nil {:print-newline? print-newline?})]
     (parse-print tokens node [])))
  ([tokens node values]
   (if (or (empty? tokens) (is-end-delimiter? (first tokens)))
     [(assoc node :value values) tokens]
     (let [[current & rest] tokens]
       ;; Print statements specifics. Extra semicolons get ignored and comma's are
       ;; a tabulator margin.
       (case (:type current)
         :semicolon (parse-print rest node values)
         :comma (parse-print rest node (conj values (->Node :tab-margin nil nil nil)))
         (let [[value tokens] (parse-node tokens)]
           (parse-print tokens node (conj values value))))))))

(defn- parse-jump
  "Parse the GOTO and GOSUB statements.

  Examples:
    GOTO 10
    GOSUB 20
  "
  [tokens label type]
  (let [node (->Node type label nil nil)
        [value tokens] (expect-and-parse tokens [:integer])
        tokens (expect-end tokens)]
    [(assoc node :value value) tokens]))

;; TODO: parse expressions
(defn- parse-expr [tokens label])

(defn- parse-node
  ([tokens] (parse-node tokens nil))
  ([tokens label]
   (if-let [[{current-type :type current-value :value} & rest] tokens]
     (case current-type
       :print (parse-print rest label)
       :comment [(->Node :noop label nil nil) rest]
       :string [(->Node :string label current-value nil) rest]
       (:goto :gosub) (parse-jump rest label current-type)
       (:ident :integer :float) [(->Node current-type label current-value nil) rest]
       :colon (parse-node rest label)
       :newline [nil rest])
     [nil []])))

(defn- parse-line
  ([tokens]
   (let [[{current-type :type current-value :value} & rest] tokens]
     ;; If the first token of the line is an integer, set the label to that
     ;; number.
     (if (= current-type :integer)
       (parse-line rest [] current-value)
       (parse-line tokens [] nil))))
  ([tokens nodes label]
   (let [[{current-type :type} & rest] tokens]
     (if (or (empty? tokens) (= current-type :newline))
       (if (= (count nodes) 1)
         [(nth nodes 0) rest]
         [(->NodeList label nodes) rest])
       (let [[node tokens] (parse-node tokens label)]
         (parse-line tokens (conj nodes node) label))))))

(defn parse
  ([tokens] (parse tokens []))
  ([tokens ast]
   (if (empty? tokens)
     ast
     (let [[node tokens] (parse-line tokens)]
       (if-not (nil? node)
         (parse tokens (conj ast node))
         (parse tokens ast))))))
