(ns basically.core
  (:gen-class)
  (:require [basically.lexer :refer [lex]]
            [basically.parser :refer [direct-statement?]]
            [basically.eval :refer [run-program run-file]]
            [basically.env :as env]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]))

(defmacro proj-version []
  (some->> (slurp "project.clj")
           edn/read-string
           (drop 1)
           (take 2)
           (str/join " ")))

(def cli-options
  [["-v" "--version" "Print basically version"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Usage: basically [options] [file]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn repl []
  (println "READY.")
  (loop [env (env/init)]
    (flush)
    (let [line (read-line)]
      (try
        (if (direct-statement? (lex line))
          (do
            (run-program line env)
            (println)
            (println "READY."))
          (env/append-program! env (str line "\n")))
        (catch Exception e
          (println (.getMessage e))
          (println)))
      (recur env))))

(defn -main [& args]
  (let [{:keys [options arguments summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      (:version options) (exit 0 (proj-version))
      (= 1 (count arguments)) (run-file (first arguments))
      (= 0 (count arguments)) (repl)
      :else (exit 1 (usage summary)))))
