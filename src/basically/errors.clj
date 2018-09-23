(ns basically.errors)

(defn error [type & [label]]
  (let [message (case type
                  :type-mismatch "?TYPE MISMATCH ERROR"
                  :syntax-error "?SYNTAX ERROR")]
    (throw (Exception. (str message (when label (str " IN " label)))))))
