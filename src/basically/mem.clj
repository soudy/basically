(ns basically.mem
  (:require [basically.funcs :refer [functions]])
  (:use [clojure.string :only [lower-case]]))

(def initial-memory
  {:variables {}        ; Name and value of current variables
   :functions functions ; Name and body of current functions
   :program ""          ; Current program
   :jump-line nil       ; Line number during GOSUB, GOTO and RUN
   :loop-stack []       ; For loop stack
   :end? false})        ; End program

(defn init []
  (atom initial-memory))

(defn clear! [mem]
  (reset! mem initial-memory))

(defn set-var! [mem name value]
  (swap! mem assoc-in [:variables (lower-case name)] value))

(defn get-var [mem name]
  (get (:variables @mem) (lower-case name) 0))

(defn set-func! [mem name body]
  (swap! mem assoc-in [:functions (lower-case name)] body))

(defn get-func [mem name]
  (get (:functions @mem) (lower-case name) nil))

(defn append-program! [mem line]
  (swap! mem update-in [:program] str line))

(defn get-program [mem]
  (:program @mem))

(defn set-jump! [mem line]
  (swap! mem assoc :jump-line line))

(defn clear-jump! [mem]
  (swap! mem assoc :jump-line nil))

(defn get-jump [mem]
  (:jump-line @mem))

(defn in-loop-stack? [mem ident]
  )

(defn end? [mem]
  (:end? @mem))

(defn set-end! [mem]
  (swap! mem assoc :end? true))

(defn clear-end! [mem]
  (swap! mem assoc :end? false))
