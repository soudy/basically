(ns basically.parser-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all])
  (:import (basically.parser NodeList Node Expr)))

(deftest parse-print-strings
  (let [tokens (lex "10 PRINT \"Hello, world\"
20 PRINT \"Goodbye, world\";
30 ? \"A\",\"B\",\"C\"")
        ast (parse tokens)]
    (is (= ast
           [(map->Node {:label "10"
                        :type :print
                        :value [(map->Node {:label nil
                                            :type :string
                                            :value "Hello, world"})]})
            (map->Node {:label "20"
                        :type :print
                        :value [(map->Node {:label nil
                                            :type :string
                                            :value "Goodbye, world"})
                                (map->Node {:label nil
                                            :type :nobreak
                                            :value nil})]})
            (map->Node {:label "30"
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
                                            :value "C"})]})]))))

(deftest parse-comment
  (let [tokens (lex "10 REM I don't do anything
20 REM But I do get parsed because Ι have a label")
        ast (parse tokens)]
    (is (= ast [(map->Node {:label "10"
                            :type :noop
                            :value nil})
                (map->Node {:label "20"
                            :type :noop
                            :value nil})]))))

(deftest parse-jump-statements
  (let [tokens (lex "10 GOTO 20
20 GOSUB 30
30 RETURN")
        ast (parse tokens)]
    (is (= ast
           [(map->Node {:label "10"
                        :type :goto
                        :value (map->Node {:label nil
                                           :type :integer
                                           :value "20"})})
            (map->Node {:label "20"
                        :type :gosub
                        :value (map->Node {:label nil
                                           :type :integer
                                           :value "30"})})
            (map->Node {:label "30"
                        :type :return
                        :value nil})]))))

(deftest parse-multiple-statements
  (let [tokens (lex "10 PRINT \"Multiple \"; : PRINT \"Statements on a line\"")
        ast (parse tokens)]
    (is (= ast
           [(map->NodeList
             {:label "10"
              :nodes [(map->Node {:type :print
                                  :label "10"
                                  :value [(map->Node {:type :string
                                                      :label nil
                                                      :value "Multiple "})
                                          (map->Node {:type :nobreak
                                                      :label nil
                                                      :value nil})]})
                      (map->Node {:type :print
                                  :label "10" :value
                                  [(map->Node {:type :string
                                               :label nil
                                               :value "Statements on a line"})]})]})]))))

(deftest parse-expressions
  (let [tokens (lex "10 A%=2 + 2 * 10
20 PRINT 2^32 * (2 + 5 - (3))")
        ast (parse tokens)]
    (is (= ast
           [(map->Node
             {:type :expr
              :label "10"
              :value (map->Expr
                      {:operator :=
                       :lhs (map->Node {:type :ident :label nil :value "A%"})
                       :rhs (map->Expr {:operator :+
                                        :lhs (map->Node {:type :integer
                                                         :label nil
                                                         :value "2"})
                                        :rhs (map->Expr
                                              {:operator :*
                                               :lhs (map->Node {:type :integer
                                                                :label nil
                                                                :value "2"})
                                               :rhs (map->Node {:type :integer
                                                                :label nil
                                                                :value
                                                                "10"})})})})})

            (map->Node
             {:type :print
              :label "20"
              :value [(map->Node {:type :expr
                                  :label nil,
                                  :value (map->Expr
                                          {:operator :*
                                           :lhs (map->Expr {:operator :exp
                                                            :lhs (map->Node
                                                                  {:type :integer
                                                                   :label nil
                                                                   :value "2"})
                                                            :rhs (map->Node
                                                                  {:type :integer
                                                                   :label nil
                                                                   :value "32"})})
                                           :rhs (map->Expr {:operator :-
                                                            :lhs (map->Expr
                                                                  {:operator :+
                                                                   :lhs (map->Node
                                                                         {:type :integer
                                                                          :label nil
                                                                          :value "2"})
                                                                   :rhs (map->Node
                                                                         {:type :integer
                                                                          :label nil
                                                                          :value "5"})})
                                                            :rhs (map->Node {:type :integer
                                                                             :label nil
                                                                             :value "3"})})})})]})]))))
