(ns basically.eval-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]
            [basically.eval :refer :all]
            [basically.mem :as mem])
  (:refer-clojure :exclude [eval]))

(deftest eval-print-statement
  (let [stdout (-> "5 A=1
10 PRINT \"Hello, world \";
20 PRINT \"Goodbye, world\"
30 PRINT A;2,3
40 PRINT \"Once, \"; :PRINT \"twice.\"" lex parse eval with-out-str)]
    (is (= "Hello, world Goodbye, world\n 1  2          3\nOnce, twice.\n" stdout))))

(deftest eval-expression
  (let [stdout (-> "10 PRINT 2 * (5 + 10)
15 PRINT +9 *(-20)
20 PRINT \"PEAR\" <> \"APPLE\" AND \"YELLOW\" <> \"BLUE\""
                   lex parse eval with-out-str)]
    (is (= " 30\n -180\n -1\n" stdout))))

(deftest eval-assignment
  (let [mem (-> "10 LET A=20*20
20 B$=\"100\"+\"100\"
30 C=A/2" lex parse eval)]
    (are [x y] (= x y)
      (mem/get-var mem "A") 400
      (mem/get-var mem "B$") "100100"
      (mem/get-var mem "C") 200)))

(deftest eval-function-call
  (let [mem (-> "10 A=SQR(25) + 10
20 B=LEFT$(\"Good morning!\", 4)" lex parse eval)]
    (are [x y] (= x y)
      (mem/get-var mem "A") 15
      (mem/get-var mem "B") "Good")))

(deftest eval-input-statement
  (let [mem (mem/init)
        program "10 INPUT \"Please give me 3 numbers\"; A%, B%, C%"
        stdout (with-in-str "4\n10.5\n500"
                 (-> program
                     lex
                     parse
                     (eval mem)
                     with-out-str))]
    (are [x y] (= x y)
      (mem/get-var mem "A%") 4
      (mem/get-var mem "B%") 10.5
      (mem/get-var mem "C%") 500
      stdout "Please give me 3 numbers? ?? ?? ")))

(deftest eval-if-statement
  (let [stdout (-> "10 IF 5 = 5 THEN PRINT \"5 equals 5\"
15 IF 5 THEN PRINT \"Conditions not equal to 0 are true\"
20 IF 5 <> 5 THEN PRINT \"Something is wrong...\"" lex parse eval with-out-str)]
    (is (= "5 equals 5\nConditions not equal to 0 are true\n" stdout))))

(deftest eval-end-statement
  (let [stdout (-> "10 END
20 PRINT \"I'm not here\"" lex parse eval with-out-str)]
    (is (= "" stdout))))

(deftest eval-if-goto-statement
  (let [stdout (-> "10 IF 5 = 5 GOTO 30
20 END
30 IF 1 <> 2 GOTO 50
40 END
50 PRINT \"Got here\"" lex parse eval with-out-str)]
    (is (= "Got here\n" stdout))))

(deftest eval-goto-statement
  (let [stdout (-> "10 GOTO 50
20 PRINT 12
50 GOTO 60
60 PRINT \"Sixty!\"" lex parse eval with-out-str)]
    (is (= "Sixty!\n" stdout))))

(deftest eval-for-loop
  (let [stdout (-> "10 FOR I=0 TO 10
20 PRINT I
30 NEXT
40 PRINT \"END\"" lex parse eval with-out-str)]
    (is (= " 0\n 1\n 2\n 3\n 4\n 5\n 6\n 7\n 8\n 9\n 10\nEND\n" stdout))))

(deftest eval-for-loop-step
  (let [stdout (-> "10 FOR I=5 TO 10 STEP 0.5 : PRINT I : NEXT"
                   lex parse eval with-out-str)]
    (is (= " 5\n 5.5\n 6\n 6.5\n 7\n 7.5\n 8\n 8.5\n 9\n 9.5\n 10\n" stdout))))

(deftest eval-run-statement
  (let [mem (mem/init)
        _ (mem/append-program! mem "10 PRINT \"Don't run me!\"\n
20 PRINT \"Start!\"")
        stdout (-> "RUN 20" lex parse (eval mem) with-out-str)]
    (is (= "Start!\n" stdout))))

(deftest eval-factorial-program
  (let [stdout (-> "
10 REM FACTORIAL CALC USING SIMPLE LOOP
20 N=10 : F=1
30 FOR I=1 TO N
40   F = F*I
50 NEXT
60 PRINT N\"! =\"F
" lex parse eval with-out-str)]
    (is (= " 10 ! = 3628800\n" stdout))))

(deftest eval-def-statement
  (let [stdout (-> "10 DEF FN SQUARE(X) = X * X
20 PRINT FN SQUARE(10)
30 PRINT SQUARE(10)" lex parse eval with-out-str)]
    (is (= " 100\n 0\n" stdout))))

(deftest eval-error-message-with-label
  (is (thrown-with-msg? Exception #"\?UNDEF'D STATEMENT ERROR IN 10"
                        (-> "10 GOTO 40" lex parse eval with-out-str))))

(deftest eval-expr-error-message-with-label
  (is (thrown-with-msg? Exception #"\?TYPE MISMATCH ERROR IN 10"
                        (-> "10 PRINT 2 + \"2\"" lex parse eval with-out-str))))

(deftest eval-collatz-conjecture-program
  (let [stdout (-> "
10 REM COLLATZ CONJECTURE
20 N = 12
30 PRINT N
35 IF N = 1 THEN END
40 IF INT(N / 2) * 2 = N THEN N = N/2 : GOTO 30
50 N = 3*N+1 : GOTO 30" lex parse eval with-out-str)]
    (is (= " 12\n 6\n 3\n 10\n 5\n 16\n 8\n 4\n 2\n 1\n" stdout))))

(deftest eval-equality-expr
  (let [stdout (-> "
10 PRINT 5.0 = 5
20 PRINT 5.0000 <> 5
30 PRINT \"5\" = \"5\" AND \"5.0\" <> \"5\"" lex parse eval with-out-str)]
    (is (= " -1\n 0\n -1\n" stdout))))

(deftest eval-syntax-error-on-wrong-arity
  (is (thrown-with-msg? Exception #"\?SYNTAX ERROR IN 10"
                        (-> "10 ABS()" lex parse eval with-out-str))))
