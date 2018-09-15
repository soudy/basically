(ns basically.parser-test
  (:require [clojure.test :refer :all]
            [basically.lexer :refer :all]
            [basically.parser :refer :all]))

(deftest parse-print-strings
  (let [tokens (lex "10 PRINT \"Hello, world\"
20 PRINT \"Goodbye, world\";
30 PRINT \"A\",\"B\",\"C\"")
        ast (parse tokens)]
    (is (= ast
           [#basically.ast.Node{:label "10"
                                :type :print
                                :opts {:print-newline? true}
                                :value [#basically.ast.Node{:label nil
                                                            :type :string
                                                            :opts nil
                                                            :value "Hello, world"}]}
            #basically.ast.Node{:label "20"
                                :type :print
                                :opts {:print-newline? false}
                                :value [#basically.ast.Node{:label nil
                                                            :type :string
                                                            :opts nil
                                                            :value "Goodbye, world"}]}
            #basically.ast.Node{:label "30"
                                :type :print
                                :opts {:print-newline? true}
                                :value [#basically.ast.Node{:label nil
                                                            :type :string
                                                            :opts nil
                                                            :value "A"}
                                        #basically.ast.Node{:label nil
                                                            :type :tab-margin
                                                            :opts nil
                                                            :value nil}
                                        #basically.ast.Node{:label nil
                                                            :type :string
                                                            :opts nil
                                                            :value "B"}
                                        #basically.ast.Node{:label nil
                                                            :type :tab-margin
                                                            :opts nil
                                                            :value nil}
                                        #basically.ast.Node{:label nil
                                                            :type :string
                                                            :opts nil
                                                            :value "C"}]}]))))

(deftest parse-comment
  (let [tokens (lex "10 REM I don't do anything
20 REM But I do get parsed because Ι have a label")
        ast (parse tokens)]
    (is (= ast [#basically.ast.Node{:label "10"
                                    :type :noop
                                    :opts nil
                                    :value nil}
                #basically.ast.Node{:label "20"
                                    :type :noop
                                    :opts nil
                                    :value nil}]))))

(deftest parse-multiple-statements
  (let [tokens (lex "10 PRINT \"Multiple\"; : PRINT \"Statements on a line\"")]
    ()))
