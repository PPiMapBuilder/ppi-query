(ns ppi-query.utils
  (:require [clojure.spec :as s]))

(defn merge-distinct [map1 map2]
  "Merge nested map recursively and concat seqable leaf into a set."
  (merge-with
    (fn [ & [a b :as args]]
      (cond
        (every? map? args) (merge-distinct a b)
        (every? seqable? args) (into #{} (concat a b))
        :else (or b a)))
    map1 map2))

(s/fdef merge-distinct
  :args (s/cat :map1 (s/nilable map?) :map2 (s/nilable map?))
  :ret (s/nilable map?))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))
