(ns basically.errors)

(defn error
  ([type] (error type nil))
  ([type label]
   (let [message (case type
                   :type-mismatch "?TYPE MISMATCH ERROR"
                   :syntax-error "?SYNTAX ERROR"
                   :undefd-statement "?UNDEF'D STATEMENT ERROR"
                   :next-without-for "?NEXT WITHOUT FOR ERROR"
                   :return-without-gosub "?RETURN WITHOUT GOSUB ERROR"
                   :undefd-function "?UNDEF'D FUNCTION ERROR"
                   :division-by-zero "?DIVISION BY ZERO ERROR")]
     (throw (Exception. (str message (when label (str " IN " label))))))))

(defn error-with-mem [type mem]
  (let [label (:current-label @mem)]
    (error type label)))
