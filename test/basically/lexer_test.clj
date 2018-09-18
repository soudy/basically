(ns basically.lexer-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all])
  (:import (basically.lexer Token)))

(deftest lex-integers
  (is (= (lex "10 2 0 500 999999999")
         [(map->Token {:type :integer :value "10"})
          (map->Token {:type :integer :value "2"})
          (map->Token {:type :integer :value "0"})
          (map->Token {:type :integer :value "500"})
          (map->Token {:type :integer :value "999999999"})])))

(deftest lex-floats
  (is (= (lex "0.0 1.234 1. .5")
         [(map->Token {:type :float :value "0.0"})
          (map->Token {:type :float :value "1.234"})
          (map->Token {:type :float :value "1."})
          (map->Token {:type :float :value ".5"})])))

(deftest lex-comment
  (is (= (lex "REM This does something important!
               REM Do not touch!
               REM")
         [(map->Token {:type :comment :value " This does something important!"})
          (map->Token {:type :newline :value \newline})
          (map->Token {:type :comment :value " Do not touch!"})
          (map->Token {:type :newline :value \newline})
          (map->Token {:type :comment :value ""})])))

(deftest lex-identifiers
  (is (= (lex "A XYZ$ point1 tmp I%")
         [(map->Token {:type :ident :value "A"})
          (map->Token {:type :ident :value "XYZ$"})
          (map->Token {:type :ident :value "point1"})
          (map->Token {:type :ident :value "tmp"})
          (map->Token {:type :ident :value "I%"})])))

(deftest lex-keywords
  (is (= (lex "LET IF THEN FOR TO STEP next WHILE WEND
               REPEAT UNTIL DO LOOP GOTO GOSUB ON def FN END
               PRINT AND OR RETURN input CLR DATA READ GET STOP NEW")
         [(map->Token {:type :let :value "LET"})
          (map->Token {:type :if :value "IF"})
          (map->Token {:type :then :value "THEN"})
          (map->Token {:type :for :value "FOR"})
          (map->Token {:type :to :value "TO"})
          (map->Token {:type :step :value "STEP"})
          (map->Token {:type :next :value "next"})
          (map->Token {:type :while :value "WHILE"})
          (map->Token {:type :wend :value "WEND"})
          (map->Token {:type :newline :value \newline})

          (map->Token {:type :repeat :value "REPEAT"})
          (map->Token {:type :until :value "UNTIL"})
          (map->Token {:type :do :value "DO"})
          (map->Token {:type :loop :value "LOOP"})
          (map->Token {:type :goto :value "GOTO"})
          (map->Token {:type :gosub :value "GOSUB"})
          (map->Token {:type :on :value "ON"})
          (map->Token {:type :def :value "def"})
          (map->Token {:type :fn :value "FN"})
          (map->Token {:type :end :value "END"})
          (map->Token {:type :newline :value \newline})

          (map->Token {:type :print :value "PRINT"})
          (map->Token {:type :and :value "AND"})
          (map->Token {:type :or :value "OR"})
          (map->Token {:type :return :value "RETURN"})
          (map->Token {:type :input :value "input"})
          (map->Token {:type :clr :value "CLR"})
          (map->Token {:type :data :value "DATA"})
          (map->Token {:type :read :value "READ"})
          (map->Token {:type :get :value "GET"})
          (map->Token {:type :stop :value "STOP"})
          (map->Token {:type :new :value "NEW"})])))

(deftest lex-operators
  (is (= (lex "- + * / < <= = >= > <>")
         [(map->Token {:type :- :value "-"})
          (map->Token {:type :+ :value "+"})
          (map->Token {:type :* :value "*"})
          (map->Token {:type :/ :value "/"})
          (map->Token {:type :< :value "<"})
          (map->Token {:type :<= :value "<="})
          (map->Token {:type := :value "="})
          (map->Token {:type :>= :value ">="})
          (map->Token {:type :> :value ">"})
          (map->Token {:type :<> :value "<>"})])))

(deftest lex-strings
  (is (= (lex "\"Hello, world\" \"\" \" \\\" \"")
         [(map->Token {:type :string :value "Hello, world"})
          (map->Token {:type :string :value ""})
          (map->Token {:type :string :value " \\\" "})])))

(deftest lex-symbols
  (is (= (lex "; : ( ) , ?") [(map->Token {:type :semicolon :value \;})
                              (map->Token {:type :colon :value \:})
                              (map->Token {:type :lparen :value \(})
                              (map->Token {:type :rparen :value \)})
                              (map->Token {:type :comma :value \,})
                              (map->Token {:type :print :value \?})])))

(deftest lex-program-factorial
  ;; https://www.rosettacode.org/wiki/Factorial#Commodore_BASIC
  (let [program "10 REM FACTORIAL
20 REM COMMODORE BASIC 2.0
30 N = 10 : GOSUB 100
40 PRINT N\"! =\"F
50 END
100 REM FACTORIAL CALC USING SIMPLE LOOP
110 F = 1
120 FOR I=1 TO N
130   F = F*I
140 NEXT
150 RETURN"]
    (is (= (lex program)
           [(map->Token {:type :integer :value "10"})
            (map->Token {:type :comment :value " FACTORIAL"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "20"})
            (map->Token {:type :comment :value " COMMODORE BASIC 2.0"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "30"})
            (map->Token {:type :ident :value "N"})
            (map->Token {:type := :value "="})
            (map->Token {:type :integer :value "10"})
            (map->Token {:type :colon :value \:})
            (map->Token {:type :gosub :value "GOSUB"})
            (map->Token {:type :integer :value "100"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "40"})
            (map->Token {:type :print :value "PRINT"})
            (map->Token {:type :ident :value "N"})
            (map->Token {:type :string :value "! ="})
            (map->Token {:type :ident :value "F"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "50"})
            (map->Token {:type :end :value "END"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "100"})
            (map->Token {:type :comment :value " FACTORIAL CALC USING SIMPLE LOOP"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "110"})
            (map->Token {:type :ident :value "F"})
            (map->Token {:type := :value "="})
            (map->Token {:type :integer :value "1"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "120"})
            (map->Token {:type :for :value "FOR"})
            (map->Token {:type :ident :value "I"})
            (map->Token {:type := :value "="})
            (map->Token {:type :integer :value "1"})
            (map->Token {:type :to :value "TO"})
            (map->Token {:type :ident :value "N"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "130"})
            (map->Token {:type :ident :value "F"})
            (map->Token {:type := :value "="})
            (map->Token {:type :ident :value "F"})
            (map->Token {:type :* :value "*"})
            (map->Token {:type :ident :value "I"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "140"})
            (map->Token {:type :next :value "NEXT"})
            (map->Token {:type :newline :value \newline})

            (map->Token {:type :integer :value "150"})
            (map->Token {:type :return :value "RETURN"})]))))
