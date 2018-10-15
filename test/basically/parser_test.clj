(ns basically.parser-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]))

(deftest parse-print-strings
  (let [ast (-> "10 PRINT \"Hello, world\"
20 PRINT \"Goodbye, world\";
30 ? \"A\",\"B\",\"C\"" lex parse)]
    (is (= [(map->Node {:label 10
                        :type :print
                        :value [(map->Node {:label nil
                                            :type :string
                                            :value "Hello, world"})]})
            (map->Node {:label 20
                        :type :print
                        :value [(map->Node {:label nil
                                            :type :string
                                            :value "Goodbye, world"})
                                (map->Node {:label nil
                                            :type :nobreak
                                            :value nil})]})
            (map->Node {:label 30
                        :type :print
                        :value [(map->Node {:label nil
                                            :type :string
                                            :value "A"})
                                (map->Node {:label nil
                                            :type :tab-margin
                                            :value nil})
                                (map->Node {:label nil
                                            :type :string
                                            :value "B"})
                                (map->Node {:label nil
                                            :type :tab-margin
                                            :value nil})
                                (map->Node {:label nil
                                            :type :string
                                            :value "C"})]})]
           ast))))

(deftest parse-comment
  (let [ast (-> "10 REM I don't do anything
20 REM But I do get parsed because Ι have a label" lex parse)]
    (is (= [(map->Node {:label 10
                        :type :noop
                        :value nil})
            (map->Node {:label 20
                        :type :noop
                        :value nil})]
           ast))))

(deftest parse-jump-statements
  (let [ast (-> "10 GOTO 20
20 GOSUB 30
30 RETURN" lex parse)]
    (is (= [(map->Node {:label 10
                        :type :goto
                        :value (map->Node {:label nil
                                           :type :integer
                                           :value 20})})
            (map->Node {:label 20
                        :type :gosub
                        :value (map->Node {:label nil
                                           :type :integer
                                           :value 30})})
            (map->Node {:label 30
                        :type :return
                        :value nil})]
           ast))))

(deftest parse-multiple-statements
  (let [ast (-> "10 PRINT \"Multiple \"; : PRINT \"Statements on a line\"" lex parse)]
    (is (= [(map->Node {:type :print
                        :label 10
                        :value [(map->Node {:type :string
                                            :label nil
                                            :value "Multiple "})
                                (map->Node {:type :nobreak
                                            :label nil
                                            :value nil})]})
            (map->Node {:type :print
                        :label 10
                        :value [(map->Node {:type :string
                                            :label nil
                                            :value "Statements on a line"})]})]
           ast))))

(deftest parse-expressions
  (let [ast (-> "10 A%=2 + 2 * -10
20 PRINT 2/32 * (2 + 5 - (3))" lex parse)]
    (is (= [(map->Node
             {:type :expr
              :label 10
              :value (map->Expr
                      {:operator :=
                       :lhs (map->Node {:type :ident :label nil :value "A%"})
                       :rhs (map->Expr {:operator :+
                                        :lhs (map->Node {:type :integer
                                                         :label nil
                                                         :value 2})
                                        :rhs (map->Expr
                                              {:operator :*
                                               :lhs (map->Node {:type :integer
                                                                :label nil
                                                                :value 2})
                                               :rhs (map->Expr
                                                     {:operator :unary-
                                                      :lhs nil
                                                      :rhs (map->Node {:type :integer
                                                                       :label nil
                                                                       :value 10})})})})})})

            (map->Node
             {:type :print
              :label 20
              :value [(map->Node {:type :expr
                                  :label nil
                                  :value (map->Expr
                                          {:operator :*
                                           :lhs (map->Expr {:operator :/
                                                            :lhs (map->Node
                                                                  {:type :integer
                                                                   :label nil
                                                                   :value 2})
                                                            :rhs (map->Node
                                                                  {:type :integer
                                                                   :label nil
                                                                   :value 32})})
                                           :rhs (map->Expr {:operator :-
                                                            :lhs (map->Expr
                                                                  {:operator :+
                                                                   :lhs (map->Node
                                                                         {:type :integer
                                                                          :label nil
                                                                          :value 2})
                                                                   :rhs (map->Node
                                                                         {:type :integer
                                                                          :label nil
                                                                          :value 5})})
                                                            :rhs (map->Node {:type :integer
                                                                             :label nil
                                                                             :value 3})})})})]})]
           ast))))

(deftest parse-function-expression
  (let [ast (-> "10 PRINT ABS(10) + FN SQRT(3.5 * 10)" lex parse)]
    (is (= [(map->Node
             {:type :print
              :label 10
              :value [(map->Node
                       {:type :expr
                        :label nil
                        :value (map->Expr
                                {:operator :+
                                 :lhs (map->FuncCall
                                       {:name "ABS"
                                        :args [(map->Node {:type :integer
                                                           :label nil
                                                           :value 10})]
                                        :user-function? false})
                                 :rhs (map->FuncCall
                                       {:name "SQRT"
                                        :args [(map->Node
                                                {:type :expr
                                                 :label nil
                                                 :value (map->Expr {:operator :*
                                                                    :lhs (map->Node {:type :float
                                                                                     :label nil
                                                                                     :value 3.5})
                                                                    :rhs (map->Node {:type :integer
                                                                                     :label nil
                                                                                     :value 10})})})]
                                        :user-function? true})})})]})]
           ast))))

(deftest parse-if-statement
  (let [ast (-> "10 IF A < B AND B < C THEN 20
20 IF A=\"\" OR B=\"\" GOTO 30" lex parse)]
    (is (= [(map->Node
             {:type :if
              :label 10
              :value (map->IfStmt
                      {:condition (map->Node
                                   {:type :expr
                                    :label nil
                                    :value (map->Expr
                                            {:operator :and
                                             :lhs (map->Expr {:operator :<
                                                              :lhs (map->Node {:type :ident
                                                                               :label nil
                                                                               :value "A"})
                                                              :rhs (map->Node {:type :ident
                                                                               :label nil
                                                                               :value "B"})})
                                             :rhs (map->Expr {:operator :<
                                                              :lhs (map->Node {:type :ident
                                                                               :label nil
                                                                               :value "B"})
                                                              :rhs (map->Node {:type :ident
                                                                               :label nil
                                                                               :value "C"})})})})
                       :body [(map->Node {:type :integer
                                          :label nil
                                          :value 20})]})})
            (map->Node
             {:type :if
              :label 20
              :value
              (map->IfStmt
               {:condition (map->Node {:type :expr
                                       :label nil
                                       :value (map->Expr
                                               {:operator :or
                                                :lhs (map->Expr {:operator :=
                                                                 :lhs (map->Node {:type :ident
                                                                                  :label nil
                                                                                  :value "A"})
                                                                 :rhs (map->Node {:type :string
                                                                                  :label nil
                                                                                  :value ""})})
                                                :rhs (map->Expr {:operator :=
                                                                 :lhs (map->Node {:type :ident
                                                                                  :label nil
                                                                                  :value "B"})
                                                                 :rhs (map->Node {:type :string
                                                                                  :label nil
                                                                                  :value ""})})})})
                :body [(map->Node {:type :goto
                                   :label nil
                                   :value (map->Node {:type :integer
                                                      :label nil
                                                      :value 30})})]})})]
           ast))))

(deftest parse-input-statement
  (let [ast (-> "10 INPUT \"How many? \"; A%
30 INPUT \"Two things please \"; A$, B$
20 INPUT A, B, C" lex parse)]
    (is (= [(map->Node {:type :input
                        :label 10
                        :value (map->InputStmt {:message "How many? "
                                                :variables [(map->Node {:type :ident
                                                                        :label nil
                                                                        :value "A%"})]})})
            (map->Node {:type :input
                        :label 20
                        :value (map->InputStmt {:message nil
                                                :variables [(map->Node {:type :ident
                                                                        :label nil
                                                                        :value "A"})
                                                            (map->Node {:type :ident
                                                                        :label nil
                                                                        :value "B"})
                                                            (map->Node {:type :ident
                                                                        :label nil
                                                                        :value "C"})]})})
            (map->Node {:type :input
                        :label 30
                        :value (map->InputStmt {:message "Two things please "
                                                :variables [(map->Node {:type :ident
                                                                        :label nil
                                                                        :value "A$"})
                                                            (map->Node {:type :ident
                                                                        :label nil
                                                                        :value "B$"})]})})]
           ast))))

(deftest parse-def-statement
  (let [ast (-> "10 DEF FN SQUARE(X) = X * X" lex parse)]
    (is (= [(map->Node
             {:type :def
              :label 10
              :value (map->DefineFunc
                      {:name "SQUARE"
                       :arg "X"
                       :body (map->Node {:type :expr
                                         :label nil
                                         :value (map->Expr {:operator :*
                                                            :lhs (map->Node {:type :ident
                                                                             :label nil
                                                                             :value "X"})
                                                            :rhs (map->Node {:type :ident
                                                                             :label nil
                                                                             :value "X"})})})})})]
           ast))))

(deftest parse-let-statement
  (let [ast (-> "10 LET A=\"BASIC RULES!\"
20 LET TAU = PI * 2" lex parse)]
    (is (= [(map->Node
             {:type :let
              :label 10
              :value (map->LetStmt
                      {:name "A"
                       :value (map->Node {:type :expr
                                          :label nil
                                          :value (map->Node {:type :string
                                                             :label nil
                                                             :value "BASIC RULES!"})})})})
            (map->Node
             {:type :let
              :label 20
              :value (map->LetStmt
                      {:name "TAU"
                       :value (map->Node {:type :expr
                                          :label nil
                                          :value (map->Expr {:operator :*
                                                             :lhs (map->Node {:type :ident
                                                                              :label nil
                                                                              :value "PI"})
                                                             :rhs (map->Node {:type :integer
                                                                              :label nil
                                                                              :value 2})})})})})]
           ast))))

(deftest parse-for-loop
  (let [ast (-> "10 FOR I=1 TO 100
20 PRINT I * 2
30 NEXT" lex parse)]
    (is (= [(map->Node
             {:type :for
              :label 10
              :value (map->ForLoop {:counter "I"
                                    :counter-value 1
                                    :to (map->Node {:type :expr
                                                    :label nil
                                                    :value (map->Node {:type :integer
                                                                       :label nil
                                                                       :value 100})})
                                    :step 1})})

            (map->Node
             {:type :print
              :label 20
              :value [(map->Node {:type :expr
                                  :label nil
                                  :value (map->Expr {:operator :*
                                                     :lhs (map->Node {:type :ident
                                                                      :label nil
                                                                      :value "I"})
                                                     :rhs (map->Node {:type :integer
                                                                      :label nil
                                                                      :value 2})})})]})
            (map->Node {:type :next
                        :label 30
                        :value []})]
           ast))))

(deftest parse-run-statement
  (let [ast (-> "10 RUN
20 RUN 30" lex parse)]
    (is (= [(map->Node {:type :run
                        :label 10
                        :value nil})
            (map->Node {:type :run
                        :label 20
                        :value 30})]
           ast))))
