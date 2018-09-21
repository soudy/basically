(ns basically.core
  (:gen-class)
  (:require [basically.lexer :refer [lex]]
            [basically.parser :refer [direct-statement?]]
            [basically.eval :refer [run-program]]
            [basically.mem :refer :all]))

(defn repl []
  (println "READY.")
  (loop [mem (mem-init)]
    (flush)
    (let [line (read-line)]
      (try
        (if (direct-statement? (lex line))
          (do
            (run-program line mem)
            (println)
            (println "READY."))
          (do
            (mem-append-program! mem (str line "\n"))))
        (catch Exception e
          (println (.getMessage e))
          (println)))
      (recur mem))))

(defn -main [& args]
  (repl))
