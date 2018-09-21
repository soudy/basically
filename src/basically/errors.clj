(ns basically.errors)

(defmulti error (fn [type & label] (type label)))

(defmethod error :type-mismatch [label]
  (throw (Exception. (str "?TYPE MISMATCH ERROR" (when label (str "IN " label))))))

(defmethod error :syntax-error [label]
  (throw (Exception. (str "?SYNTAX ERROR" (when label (str "IN " label))))))
