(ns basically.eval
  (:import [basically.parser NodeList])
  (:refer-clojure :exclude [eval number?]))

(defn- number? [type] (or (= type :integer) (= type :float)))

(defn- eval-expr [expr]
  "")

(defn- eval-print-arg
  [[{:keys [type value]} & [{next-type :type} & _] :as args] mem]
  ;; Print a newline if we're at the last argument, and it's not a semicolon
  (let [print-newline? (and (= (count args) 1) (not= type :nobreak))
        value (case type
                (:integer :float) (cond
                                    (= next-type :string) (str value " ")
                                    (= next-type :semicolon) (str value "  ")
                                    :else value)
                :string (if (or (number? next-type) (= next-type :expr))
                          (str value " ")
                          value)
                :expr (eval-expr value)
                :tab-margin (apply str (repeat 10 " "))
                :nobreak "")]
    (str value (when print-newline? "\n"))))

(defn- eval-print-args
  "Evaluate the argument given to the print statement, forming a single string
  to print."
  ([[{:keys [type]} & rest :as args] mem]
   ;; If the arguments start with a float or integer, indent by a space
   (let [begin-message (if (number? type) " " "")]
     (eval-print-args args mem begin-message)))
  ([args mem message]
   (if (empty? args)
     message
     (let [arg-value (eval-print-arg args mem)]
       (recur (drop 1 args) mem (str message arg-value))))))

(defn- eval-print [[{:keys [value]} & rest] mem]
  (let [message (eval-print-args value mem)]
    (print message)
    [rest mem]))

(defn- eval-node [[{:keys [type] :as current} & _ :as ast] mem]
  (if (instance? NodeList current)
    (map #(eval-node % mem) current)
    (case type
      :print (eval-print ast mem))))

(defn eval
  "Evaluate an AST."
  ([ast] (eval ast ""))
  ([ast mem]
   (when-not (empty? ast)
     (let [[ast mem] (eval-node ast mem)]
       (recur ast mem)))))
