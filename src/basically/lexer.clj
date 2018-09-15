(ns basically.lexer)

(use '[clojure.string :only [lower-case join]])

(defrecord Token [type value])

(defn- eat
  "Eat consumes 1 or n characters."
  ([program] (subs program 1))
  ([program n] (subs program n)))

(defn- top
  "Top shows the top or top n characters of the program."
  ([program] (first program))
  ([program n] (if (< (count program) n) program (subs program 0 n))))

(let [zero (int \0)
      nine (int \9)]
  (defn- is-integer? [c] (<= zero (int c) nine)))

(let [lowercase-a (int \a) lowercase-z (int \z)
      uppercase-a (int \A) uppercase-z (int \Z)]
  (defn- is-ident? [c]
    (or (<= lowercase-a (int c) lowercase-z)
        (<= uppercase-a (int c) uppercase-z)
        (is-integer? c)
        (= c \$)
        (= c \%))))

(let [whitespace [\tab \space \return]]
  (defn- is-whitespace? [c] (some #(= c %) whitespace)))

(defn- is-comment? [program]
  (= (lower-case (top program 3)) "rem"))

(let [operators [\- \+ \* \/ \< \> \=]]
  (defn- is-operator? [c] (some #(= c %) operators)))

(let [keywords [:let :if :then :else :for :to :step :next :while :wend
                :repeat :until :do :loop :goto :gosub :on :def :fn :end
                :print :and :or :return :input :clr :data :read :get :stop]]
  (defn- get-keyword
    "Convert an identifier to a BASIC keyword, if it is a keyword."
    [ident]
    (let [keyword (keyword (lower-case ident))]
      (some #{keyword} keywords))))

(let [symbols {\; :semicolon
               \: :colon
               \( :lparen
               \) :rparen
               \, :comma
               \? :print}] ; ? is a shortcut for print
  (defn- is-symbol? [c] (some #(= c %) (keys symbols)))
  (defn- get-symbol-keyword [c] (get symbols c :error)))

(defn- is-string? [c] (= c \"))

(defn- scan-while
  "Scan the program while a condition is true. When this condition is no longer
  true, return a token with the provided token type."
  ([program condition token-type] (scan-while program condition token-type ""))
  ([program condition token-type value]
   (if (empty? program)
     [(->Token token-type value) program]
     (let [current (top program)]
       (if-not (condition current)
         [(->Token token-type value) program]
         (scan-while (eat program) condition token-type (str value current)))))))

(defn- scan-number
  "Scan a number. A number can be an integer or a float, which is decided when
  we find a `.' after scanning an integer. If so, it will be considered a float
  and we will scan for integers again to determine the fractional part."
  [program]
  (let [[token program] (scan-while program is-integer? :integer)]
    (if (= (top program) \.)
      ;; A dot after integer literal means a float, so scan-while again for the
      ;; fractional part and join them together to get the float value.
      (let [[decimals program] (scan-while (eat program) is-integer? :integer)
            float-value (str (:value token) "." (:value decimals))]
        [(->Token :float float-value) program])
      [token program])))

(defn- scan-ident [program]
  (let [[token program] (scan-while program is-ident? :ident)]
    (if-let [keyword (get-keyword (:value token))]
      [(assoc token :type keyword) program]
      [token program])))

(defn- scan-operator [program]
  (if-let [operator (some #{(top program 2)} ["<>" "<=" ">="])]
    [(->Token (keyword operator) operator) (eat program 2)]
    (let [operator (str (top program))]
      [(->Token (keyword operator) operator) (eat program)])))

(defn- scan-string
  ([program] (scan-string (eat program) ""))
  ([program value]
   (if (empty? program)
     [(->Token :error value) program] ; Unterminated string
     (let [current (top program)]
       (case current
         \\ (scan-string (eat program 2) (str value (top program 2)))
         \" [(->Token :string value) (eat program)]
         (scan-string (eat program) (str value current)))))))

(defn- scan-token [program]
  (let [current (top program)]
    (cond
      (is-whitespace? current) [nil (eat program)]
      (is-comment? program) (scan-while (eat program 3) #(not= % \newline) :comment)
      (or (is-integer? current) (= current \.)) (scan-number program)
      (is-ident? current) (scan-ident program)
      (is-operator? current) (scan-operator program)
      (is-string? current) (scan-string program)
      (is-symbol? current) [(->Token (get-symbol-keyword current) current)
                            (eat program)]
      (= current \newline) [(->Token :newline current) (eat program)]
      :else [(->Token :error current) (eat program)])))

(defn lex
  "Lex converts a string into a list of tokens."
  ([program] (lex program []))
  ([program tokens]
   (if (empty? program)
     tokens
     (let [[token program] (scan-token program)]
       (if-not (nil? token)
         (lex program (conj tokens token))
         ;; If we get no token, we're skipping whitespace or comment
         (lex program tokens))))))
