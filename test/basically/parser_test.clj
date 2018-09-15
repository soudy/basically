(ns basically.parser-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all])
  (:import (basically.parser NodeList Node)))

(deftest parse-print-strings
  (let [tokens (lex "10 PRINT \"Hello, world\"
20 PRINT \"Goodbye, world\";
30 ? \"A\",\"B\",\"C\"")
        ast (parse tokens)]
    (is (= ast
           [(map->Node {:label "10"
                        :type :print
                        :opts {:print-newline? true}
                        :value [(map->Node {:label nil
                                            :type :string
                                            :opts nil
                                            :value "Hello, world"})]})
            (map->Node {:label "20"
                        :type :print
                        :opts {:print-newline? false}
                        :value [(map->Node {:label nil
                                            :type :string
                                            :opts nil
                                            :value "Goodbye, world"})]})
            (map->Node {:label "30"
                        :type :print
                        :opts {:print-newline? true}
                        :value [(map->Node {:label nil
                                            :type :string
                                            :opts nil
                                            :value "A"})
                                (map->Node {:label nil
                                            :type :tab-margin
                                            :opts nil
                                            :value nil})
                                (map->Node {:label nil
                                            :type :string
                                            :opts nil
                                            :value "B"})
                                (map->Node {:label nil
                                            :type :tab-margin
                                            :opts nil
                                            :value nil})
                                (map->Node {:label nil
                                            :type :string
                                            :opts nil
                                            :value "C"})]})]))))

(deftest parse-comment
  (let [tokens (lex "10 REM I don't do anything
20 REM But I do get parsed because Ι have a label")
        ast (parse tokens)]
    (is (= ast [(map->Node {:label "10"
                                       :type :noop
                                       :opts nil
                                       :value nil})
                (map->Node {:label "20"
                                       :type :noop
                                       :opts nil
                                       :value nil})]))))

(deftest parse-jump-statements
  (let [tokens (lex "10 GOTO 20
20 GOSUB 10")
        ast (parse tokens)]
    (is (= ast
           [(map->Node {:label "10"
                        :type :goto
                        :opts nil
                        :value (map->Node {:label nil
                                           :type :integer
                                           :opts nil
                                           :value "20"})})
            (map->Node {:label "20"
                        :type :gosub
                        :opts nil
                        :value (map->Node {:label nil
                                           :type :integer
                                           :opts nil
                                           :value "10"})})]))))

(deftest parse-multiple-statements
  (let [tokens (lex "10 PRINT \"Multiple \"; : PRINT \"Statements on a line\"")
        ast (parse tokens)]
    (is (= ast
           [(map->NodeList {:label "10"
                            :nodes [(map->Node {:type :print
                                                :label "10"
                                                :value [(map->Node {:type :string
                                                                    :label nil
                                                                    :value "Multiple "
                                                                    :opts nil})]
                                                :opts {:print-newline? false}})
                                    (map->Node {:type :print
                                                :label "10" :value
                                                [(map->Node {:type :string
                                                             :label nil
                                                             :value "Statements on a line"
                                                             :opts nil})]
                                                :opts {:print-newline? true}})]})]))))

;; (deftest parse-expressions
;;   (let [tokens (lex "10 A%=2 + 2 * 10")
;;         ast (parse tokens)]
;;     (is (= ast
;;            [(map->Node {:label 10
;;                         :type :ident
;;                         :opts nil
;;                         :value "A%"})]))))
