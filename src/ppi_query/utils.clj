(ns ppi-query.utils
  (:require [clojure.spec.alpha :as s]))

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

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel
  overhead worthwhile"
  [grain-size f & colls]
  (apply concat
   (apply pmap
          (fn [& pgroups] (doall (apply map f pgroups)))
          (map (partial partition-all grain-size) colls))))


(defn ppmap2
  "Partitioned pmap over two threads (works only for one coll)"
  [f coll]
  (ppmap (+ (/ (count coll) 2) 1)
         f coll))
(defn ppmap4
  "Partitioned pmap over four threads (works only for one coll)"
  [f coll]
  (ppmap (+ (/ (count coll) 4) 1)
         f coll))

(defn trace [a & args]
  (println a args)
  a)
(defn trace-m [m a]
  (println m)
  a)
