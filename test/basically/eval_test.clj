(ns basically.eval-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]
            [basically.eval :refer :all]
            [basically.mem :refer :all])
  (:refer-clojure :exclude [eval]))

(deftest eval-print-statement
  (let [stdout (-> "10 PRINT \"Hello, world \";
20 PRINT \"Goodbye, world\"
30 PRINT 1;2,3" lex parse eval with-out-str)]
    (is (= stdout "Hello, world Goodbye, world\n 1  2          3\n"))))


(deftest eval-expression
  (let [stdout (-> "10 PRINT 2 * (5 + 10)
20 PRINT \"PEAR\" <> \"APPLE\" AND \"YELLOW\" <> \"BLUE\""
                   lex parse eval with-out-str)]
    (is (= stdout "30\n-1\n"))))

(deftest eval-assignment
  (let [mem (-> "10 A=20*20
20 B$=\"100\"+\"100\"
30 C=A/2" lex parse eval)]
    (are [x y] (= x y)
      (mem-get-var mem "A") 400
      (mem-get-var mem "B$") "100100"
      (mem-get-var mem "C") 200)))


(deftest eval-function-call
  (let [mem (-> "10 A=SQR(25) + 10
20 B=LEFT$(\"Good morning!\", 4)" lex parse eval)]
    (are [x y] (= x y)
      (mem-get-var mem "A") 15
      (mem-get-var mem "B") "Good")))

(deftest eval-input-statement
  (let [mem (mem-init)
        output (with-out-str
                 (with-in-str "4\n10\n500"
                   (eval (-> "10 INPUT \"Please give me 3 numbers\"; A%, B%, C%" lex parse) mem)))]
    (are [x y] (= x y)
      (mem-get-var mem "A%") 4
      (mem-get-var mem "B%") 10
      (mem-get-var mem "C%") 500
      output "Please give me 3 numbers? ?? ?? ")))
