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

(s/def ::map-or-nil (s/or :nil nil? :map map?))

(s/fdef merge-distinct
  :args (s/cat :map1 ::map-or-nil :map2 ::map-or-nil)
  :ret ::map-or-nil)
