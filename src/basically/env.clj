(ns basically.env
  (:require [basically.funcs :refer [functions]])
  (:use [clojure.string :only [lower-case]]))

(def ^:private initial-environment
  {:current-label nil   ; Current label number
   :variables {}        ; Name and value of current variables
   :functions functions ; Name and body of current functions
   :program ""          ; Current program
   :jump-line nil       ; Line number during GOSUB, GOTO, RUN and FOR
   :loop-stack []       ; For loop stack
   :end? false})        ; End program

(defn init []
  (atom initial-environment))

(defn clear! [env]
  (reset! env initial-environment))

(defn set-current-label! [env label]
  (swap! env assoc :current-label label))

(defn set-var! [env name value]
  (swap! env assoc-in [:variables (lower-case name)] value))

(defn get-var [env name]
  (get (:variables @env) (lower-case name) 0))

(defn define-function! [env {:keys [name param body]}]
  (swap! env assoc-in [:functions (lower-case name)] {:param param :body body}))

(defn get-func [env name]
  (get (:functions @env) (lower-case name) nil))

(defn append-program! [env line]
  (swap! env update-in [:program] str line))

(defn get-program [env]
  (:program @env))

(defn set-jump! [env line]
  (swap! env assoc :jump-line line))

(defn clear-jump! [env]
  (swap! env assoc :jump-line nil))

(defn get-jump [env]
  (:jump-line @env))

(defn in-loop-stack? [env for-loop]
  (some #{for-loop} (:loop-stack @env)))

(defn push-loop-stack! [env for-loop]
  (swap! env update-in [:loop-stack] conj for-loop))

(defn peek-loop-stack [env]
  (peek (:loop-stack @env)))

(defn pop-loop-stack! [env]
  (swap! env assoc :loop-stack (pop (:loop-stack @env))))

(defn end? [env]
  (:end? @env))

(defn set-end! [env]
  (swap! env assoc :end? true))

(defn clear-end! [env]
  (swap! env assoc :end? false))
