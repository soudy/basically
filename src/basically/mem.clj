(ns basically.mem
  (:require [basically.funcs :refer [functions]])
  (:use [clojure.string :only [lower-case]]))

(def initial-memory
  {:current-line nil    ; Current BASIC line number
   :jump-line nil       ; Line number during GOSUB, GOTO and RUN
   :variables {}        ; Name and value of current variables
   :functions functions ; Name and body of current functions
   :program ""})        ; Current program

(defn mem-init []
  (atom initial-memory))

(defn mem-reset! [mem]
  (reset! mem initial-memory))

(defn mem-set-var! [mem name value]
  (swap! mem assoc-in [:variables (lower-case name)] value))

(defn mem-get-var [mem name]
  (get (:variables @mem) (lower-case name) 0))

(defn mem-set-func! [mem name body]
  (swap! mem assoc-in [:functions (lower-case name)] body))

(defn mem-get-func [mem name]
  (get (:functions @mem) (lower-case name) nil))

(defn mem-append-program! [mem line]
  (swap! mem update-in [:program] str line))

(defn mem-get-program [mem]
  (:program @mem))
