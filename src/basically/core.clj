(ns basically.core
  (:gen-class)
  (:require [basically.lexer :refer [lex]]
            [basically.parser :refer [parse direct-statement?]]
            [basically.eval :refer [eval]]
            [basically.mem :refer :all])
  (:refer-clojure :exclude [eval]))

(defn repl []
  (println "READY.")
  (loop [mem (mem-init)]
    (flush)
    (let [line (read-line)]
      (try
        (if (direct-statement? (lex line))
          (do
            (-> line
                lex
                parse
                (eval mem))
            (println)
            (println "READY."))
          (do
            (mem-append-program! mem line)))
        (catch Exception e
          (println (.getMessage e))
          (println)))
      (recur mem))))

(defn -main [& args]
  (repl))
