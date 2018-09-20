(ns basically.eval-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]
            [basically.eval :refer :all])
  (:refer-clojure :exclude [eval]))

(deftest eval-print-statement
  (let [stdout (-> "10 PRINT \"Hello, world \";
20 PRINT \"Goodbye, world\"" lex parse eval with-out-str)]
    (is (= stdout "Hello, world Goodbye, world\n"))))


(deftest eval-expression
  (let [stdout (-> "10 PRINT 2 * (5 + 10)
20 PRINT \"PEAR\" <> \"APPLE\" AND \"YELLOW\" <> \"BLUE\""
                   lex parse eval with-out-str)]
    (is (= stdout "30\n-1\n"))))
