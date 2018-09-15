(ns basically.parser)

(defrecord Node [type label value opts])
(defrecord NodeList [label nodes])

(defn- is-end-delimiter? [{:keys [type]}]
  (or (= type :newline)
      (= type :colon)))

(declare parse-node)

(defn- print-newline?
  "Determine if we print a newline or not. If a PRINT statement ends with a `;',
  don't print a newline."
  [tokens]
  (let [args (take-while #(not (is-end-delimiter? %)) tokens)]
    (not= (:type (last args)) :semicolon)))

(defn- parse-print
  ([tokens label]
   ;; Determine if we print a newline by checking if the last token before the
   ;; end of the statement is a `;'
   (let [node (->Node :print label nil {:print-newline? (print-newline? tokens)})]
     (parse-print tokens node [])))
  ([tokens node values]
   (if (or (empty? tokens) (is-end-delimiter? (first tokens)))
     [(assoc node :value values) (drop 1 tokens)] ;; Lose delimiter
     (let [[current & rest] tokens]
       ;; Print statements specifics. Extra semicolons get ignored and comma's are
       ;; a tabulator margin.
       (case (:type current)
         :semicolon (parse-print rest node values)
         :comma (parse-print rest node (conj values (->Node :tab-margin nil nil nil)))
         (let [[value tokens] (parse-node tokens nil)]
           (parse-print tokens node (conj values value))))))))

(defn- parse-expr [tokens label])

(defn- parse-node [tokens label]
  (if-let [[current & rest] tokens]
    (case (:type current)
      :print
        (parse-print rest label)
      :comment
        [(->Node :noop label nil nil) rest]
      :string
        [(->Node :string label (:value current) nil) rest]
      (:ident :integer :float :lparen)
        (parse-expr tokens label)
      (:newline :colon)
        [nil rest])
    [nil []]))

(defn- parse-line [tokens]
  (let [[current & rest] tokens]
    (if (= (:type current) :integer)
      (parse-node rest (:value current))
      (parse-node tokens nil))))

(defn parse
  ([tokens] (parse tokens []))
  ([tokens ast]
   (if (empty? tokens)
     ast
     (let [[node tokens] (parse-line tokens)]
       (if-not (nil? node)
         (parse tokens (conj ast node))
         (parse tokens ast))))))
