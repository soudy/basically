(ns basically.lexer-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all])
  (:import basically.lexer.Token))

(deftest lex-integers
  (is (= (lex "10 2 0 500 999999999")
         [#basically.lexer.Token{:type :integer :value "10"}
          #basically.lexer.Token{:type :integer :value "2"}
          #basically.lexer.Token{:type :integer :value "0"}
          #basically.lexer.Token{:type :integer :value "500"}
          #basically.lexer.Token{:type :integer :value "999999999"}])))

(deftest lex-floats
  (is (= (lex "0.0 1.234 1. .5")
         [#basically.lexer.Token{:type :float :value "0.0"}
          #basically.lexer.Token{:type :float :value "1.234"}
          #basically.lexer.Token{:type :float :value "1."}
          #basically.lexer.Token{:type :float :value ".5"}])))

(deftest lex-comment
  (is (= (lex "REM This does something important!
               REM Do not touch!
               REM")
         [#basically.lexer.Token{:type :comment :value " This does something important!"}
          #basically.lexer.Token{:type :newline :value \newline}
          #basically.lexer.Token{:type :comment :value " Do not touch!"}
          #basically.lexer.Token{:type :newline :value \newline}
          #basically.lexer.Token{:type :comment :value ""}])))

(deftest lex-identifiers
  (is (= (lex "A XYZ$ point1 tmp I%")
         [#basically.lexer.Token{:type :ident :value "A"}
          #basically.lexer.Token{:type :ident :value "XYZ$"}
          #basically.lexer.Token{:type :ident :value "point1"}
          #basically.lexer.Token{:type :ident :value "tmp"}
          #basically.lexer.Token{:type :ident :value "I%"}])))

(deftest lex-keywords
  (is (= (lex "LET DATA IF THEN ELSE FOR TO STEP next WHILE WEND
               REPEAT UNTIL DO LOOP GOTO GOSUB ON def FN END
               PRINT CLR NOT AND OR LEN RETURN input")
         [#basically.lexer.Token{:type :let :value "LET"}
          #basically.lexer.Token{:type :data :value "DATA"}
          #basically.lexer.Token{:type :if :value "IF"}
          #basically.lexer.Token{:type :then :value "THEN"}
          #basically.lexer.Token{:type :else :value "ELSE"}
          #basically.lexer.Token{:type :for :value "FOR"}
          #basically.lexer.Token{:type :to :value "TO"}
          #basically.lexer.Token{:type :step :value "STEP"}
          #basically.lexer.Token{:type :next :value "next"}
          #basically.lexer.Token{:type :while :value "WHILE"}
          #basically.lexer.Token{:type :wend :value "WEND"}
          #basically.lexer.Token{:type :newline :value \newline}

          #basically.lexer.Token{:type :repeat :value "REPEAT"}
          #basically.lexer.Token{:type :until :value "UNTIL"}
          #basically.lexer.Token{:type :do :value "DO"}
          #basically.lexer.Token{:type :loop :value "LOOP"}
          #basically.lexer.Token{:type :goto :value "GOTO"}
          #basically.lexer.Token{:type :gosub :value "GOSUB"}
          #basically.lexer.Token{:type :on :value "ON"}
          #basically.lexer.Token{:type :def :value "def"}
          #basically.lexer.Token{:type :fn :value "FN"}
          #basically.lexer.Token{:type :end :value "END"}
          #basically.lexer.Token{:type :newline :value \newline}

          #basically.lexer.Token{:type :print :value "PRINT"}
          #basically.lexer.Token{:type :clr :value "CLR"}
          #basically.lexer.Token{:type :not :value "NOT"}
          #basically.lexer.Token{:type :and :value "AND"}
          #basically.lexer.Token{:type :or :value "OR"}
          #basically.lexer.Token{:type :len :value "LEN"}
          #basically.lexer.Token{:type :input :value "input"}
          #basically.lexer.Token{:type :return :value "RETURN"}])))

(deftest lex-operators
  (is (= (lex "- + * / < <= = >= > <>")
         [#basically.lexer.Token{:type :- :value "-"}
          #basically.lexer.Token{:type :+ :value "+"}
          #basically.lexer.Token{:type :* :value "*"}
          #basically.lexer.Token{:type :/ :value "/"}
          #basically.lexer.Token{:type :< :value "<"}
          #basically.lexer.Token{:type :<= :value "<="}
          #basically.lexer.Token{:type := :value "="}
          #basically.lexer.Token{:type :>= :value ">="}
          #basically.lexer.Token{:type :> :value ">"}
          #basically.lexer.Token{:type :<> :value "<>"}])))

(deftest lex-strings
  (is (= (lex "\"Hello, world\" \"\" \" \\\" \"")
         [#basically.lexer.Token{:type :string :value "Hello, world"}
          #basically.lexer.Token{:type :string :value ""}
          #basically.lexer.Token{:type :string :value " \\\" "}])))

(deftest lex-symbols
  (is (= (lex "; : ( ) , ?") [#basically.lexer.Token{:type :semicolon :value \;}
                              #basically.lexer.Token{:type :colon :value \:}
                              #basically.lexer.Token{:type :lparen :value \(}
                              #basically.lexer.Token{:type :rparen :value \)}
                              #basically.lexer.Token{:type :comma :value \,}
                              #basically.lexer.Token{:type :print :value \?}])))

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
           [#basically.lexer.Token{:type :integer :value "10"}
            #basically.lexer.Token{:type :comment :value " FACTORIAL"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "20"}
            #basically.lexer.Token{:type :comment :value " COMMODORE BASIC 2.0"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "30"}
            #basically.lexer.Token{:type :ident :value "N"}
            #basically.lexer.Token{:type := :value "="}
            #basically.lexer.Token{:type :integer :value "10"}
            #basically.lexer.Token{:type :colon :value \:}
            #basically.lexer.Token{:type :gosub :value "GOSUB"}
            #basically.lexer.Token{:type :integer :value "100"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "40"}
            #basically.lexer.Token{:type :print :value "PRINT"}
            #basically.lexer.Token{:type :ident :value "N"}
            #basically.lexer.Token{:type :string :value "! ="}
            #basically.lexer.Token{:type :ident :value "F"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "50"}
            #basically.lexer.Token{:type :end :value "END"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "100"}
            #basically.lexer.Token{:type :comment :value " FACTORIAL CALC USING SIMPLE LOOP"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "110"}
            #basically.lexer.Token{:type :ident :value "F"}
            #basically.lexer.Token{:type := :value "="}
            #basically.lexer.Token{:type :integer :value "1"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "120"}
            #basically.lexer.Token{:type :for :value "FOR"}
            #basically.lexer.Token{:type :ident :value "I"}
            #basically.lexer.Token{:type := :value "="}
            #basically.lexer.Token{:type :integer :value "1"}
            #basically.lexer.Token{:type :to :value "TO"}
            #basically.lexer.Token{:type :ident :value "N"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "130"}
            #basically.lexer.Token{:type :ident :value "F"}
            #basically.lexer.Token{:type := :value "="}
            #basically.lexer.Token{:type :ident :value "F"}
            #basically.lexer.Token{:type :* :value "*"}
            #basically.lexer.Token{:type :ident :value "I"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "140"}
            #basically.lexer.Token{:type :next :value "NEXT"}
            #basically.lexer.Token{:type :newline :value \newline}

            #basically.lexer.Token{:type :integer :value "150"}
            #basically.lexer.Token{:type :return :value "RETURN"}]))))
