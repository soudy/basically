(ns basically.mem
  (:require [basically.funcs :refer [functions]])
  (:use [clojure.string :only [lower-case]]))

(def ^:private initial-memory
  {:current-label nil   ; Current label number
   :variables {}        ; Name and value of current variables
   :functions functions ; Name and body of current functions
   :program ""          ; Current program
   :jump-line nil       ; Line number during GOSUB, GOTO, RUN and FOR
   :loop-stack []       ; For loop stack
   :end? false})        ; End program

(defn init []
  (atom initial-memory))

(defn clear! [mem]
  (reset! mem initial-memory))

(defn set-current-label! [mem label]
  (swap! mem assoc :current-label label))

(defn set-var! [mem name value]
  (swap! mem assoc-in [:variables (lower-case name)] value))

(defn get-var [mem name]
  (get (:variables @mem) (lower-case name) 0))

(defn define-function! [mem {:keys [name param body]}]
  (swap! mem assoc-in [:functions (lower-case name)] {:param param :body body}))

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

(defn in-loop-stack? [mem for-loop]
  (some #{for-loop} (:loop-stack @mem)))

(defn push-loop-stack! [mem for-loop]
  (swap! mem update-in [:loop-stack] conj for-loop))

(defn peek-loop-stack [mem]
  (peek (:loop-stack @mem)))

(defn pop-loop-stack! [mem]
  (swap! mem assoc :loop-stack (pop (:loop-stack @mem))))

(defn end? [mem]
  (:end? @mem))

(defn set-end! [mem]
  (swap! mem assoc :end? true))

(defn clear-end! [mem]
  (swap! mem assoc :end? false))
