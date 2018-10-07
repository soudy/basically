(ns basically.lexer
  (:require [basically.errors :refer [error]]
            [clojure.string :refer [lower-case join]])
  (:refer-clojure :exclude [integer? symbol? string?]))

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
  (defn- integer? [c] (<= zero (int c) nine)))

(let [lowercase-a (int \a) lowercase-z (int \z)
      uppercase-a (int \A) uppercase-z (int \Z)]
  (defn- ident? [c]
    (or (<= lowercase-a (int c) lowercase-z)
        (<= uppercase-a (int c) uppercase-z)
        (integer? c)
        (= c \$)
        (= c \%))))

(let [whitespace [\tab \space \return]]
  (defn- whitespace? [c] (some (partial = c) whitespace)))

(defn- comment? [program]
  (= (lower-case (top program 3)) "rem"))

(let [operators [\- \+ \* \/ \< \> \=]]
  (defn- operator? [c] (some (partial = c) operators)))

(let [keywords [:let :if :then :for :to :step :next :goto :gosub :on :def :fn
                :end :print :and :or :return :input :clr :data :read :get :stop
                :new :run]]
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
  (defn- symbol? [c] (some (partial = c) (keys symbols)))
  (defn- get-symbol-keyword [c] (get symbols c :error)))

(defn- string? [c] (= c \"))

(defn- scan-while
  "Scan the program while a condition is true. When this condition is no longer
  true, return a token with the provided token type."
  ([program condition token-type]
   (scan-while program condition token-type ""))
  ([[current :as program] condition token-type value]
   (if (or (empty? program) (not (condition current)))
     [(->Token token-type value) program]
     (recur (eat program) condition token-type (str value current)))))

(defn- scan-number
  "Scan a number. A number can be an integer or a float, which is decided when
  we find a `.' after scanning an integer. If so, it will be considered a float
  and we will scan for integers again to determine the fractional part."
  [program]
  (let [[token program] (scan-while program integer? :integer)]
    (if (= (top program) \.)
      ;; A dot after integer literal means a float, so scan-while again for the
      ;; fractional part and join them together to get the float value.
      (let [[decimals program] (scan-while (eat program) integer? :integer)
            float-value (str (:value token) "." (:value decimals))]
        [(->Token :float (read-string float-value)) program])
      [(assoc token :value (read-string (:value token))) program])))

(defn- scan-ident [program]
  (let [[token program] (scan-while program ident? :ident)]
    (if-let [keyword (get-keyword (:value token))]
      [(assoc token :type keyword) program]
      [token program])))

(defn- unary?
  "If the previous token is not an identifier, `)', string or number, it's
  a unary operator. Only `+' and `-' can be unary."
  [operator {prev-type :type}]
  (and (or (= operator \+) (= operator \-))
       (not-any? (partial = prev-type) [:ident :rparen :string :float :integer])))

(defn- scan-operator [[current :as program] prev-token]
  (if-let [operator (some #{(top program 2)} ["<>" "<=" ">="])]
    [(->Token (keyword operator) operator) (eat program 2)]
    (let [operator (if (unary? current prev-token)
                     (str "unary" current)
                     (str current))]
      [(->Token (keyword operator) (str current)) (eat program)])))

(defn- scan-string
  ([program]
   (scan-string (eat program) ""))
  ([[current :as program] value]
   (if (empty? program)
     (error :syntax-error) ; Unterminated string
     (case current
       \\ (recur (eat program 2) (str value (top program 2)))
       \" [(->Token :string value) (eat program)]
       (recur (eat program) (str value current))))))

(defn- scan-token [[current :as program] prev-token]
  (cond
    (whitespace? current) [nil (eat program)]
    (comment? program) (scan-while (eat program 3) (partial not= \newline) :comment)
    (integer? current) (scan-number program)
    (ident? current) (scan-ident program)
    (operator? current) (scan-operator program prev-token)
    (string? current) (scan-string program)
    (symbol? current) [(->Token (get-symbol-keyword current) current)
                       (eat program)]
    (= current \newline) [(->Token :newline current) (eat program)]
    :else (error :syntax-error)))

(defn lex
  "Lex converts a string into a list of tokens."
  ([program]
   (lex program []))
  ([program tokens]
   (if (empty? program)
     tokens
     (let [[token program] (scan-token program (last tokens))]
       (if-not (nil? token)
         (recur program (conj tokens token))
         ;; If we get no token, we're skipping whitespace
         (recur program tokens))))))
