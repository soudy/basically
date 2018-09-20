(ns basically.mem)

(def initial-memory
  {:current-line nil ; Current BASIC line number
   :jump-line nil    ; Line number during GOSUB, GOTO and RUN
   :variables {}     ; Name and value of current variables
   :functions {}     ; Name and body of current functions TODO: fill with system functions
   :program ""})     ; Current program

(defn mem-init []
  (atom initial-memory))

(defn mem-reset! [mem]
  (reset! mem initial-memory))

(defn mem-set-var! [mem name value]
  (swap! mem assoc-in [:variables name] value))

(defn mem-get-var [mem name]
  (get (:variables @mem) name 0))

(defn mem-append-program! [mem line]
  (swap! mem update-in [:program] str line))

(defn mem-get-program [mem]
  (:program @mem))
