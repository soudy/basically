(ns basically.eval-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]
            [basically.eval :refer :all])
  (:refer-clojure :exclude [eval]))

(deftest eval-print-statement
  (let [result (-> "10 PRINT \"Hello, world \";
20 PRINT \"Goodbye, world\"" lex parse eval)]))
