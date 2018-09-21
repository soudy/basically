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
    (is (= "Hello, world Goodbye, world\n 1  2          3\n" stdout))))


(deftest eval-expression
  (let [stdout (-> "10 PRINT 2 * (5 + 10)
20 PRINT \"PEAR\" <> \"APPLE\" AND \"YELLOW\" <> \"BLUE\""
                   lex parse eval with-out-str)]
    (is (= "30\n-1\n" stdout))))

(deftest eval-assignment
  (let [mem (-> "10 LET A=20*20
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
        program "10 INPUT \"Please give me 3 numbers\"; A%, B%, C%"
        stdout (with-out-str
                 (with-in-str "4\n10.5\n500"
                   (eval (->  program lex parse) mem)))]
    (are [x y] (= x y)
      (mem-get-var mem "A%") 4
      (mem-get-var mem "B%") 10.5
      (mem-get-var mem "C%") 500
      stdout "Please give me 3 numbers? ?? ?? ")))

(deftest eval-if-statement
  (let [stdout (-> "10 IF 5 = 5 THEN PRINT \"5 equals 5\"
20 IF 5 <> 5 THEN PRINT \"Something is wrong...\"" lex parse eval with-out-str)]
    (is (= "5 equals 5\n" stdout))))
