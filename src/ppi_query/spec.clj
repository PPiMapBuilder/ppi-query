(ns ppi-query.spec
  (:require [clojure.spec :as s]
            [clojure.set :refer [union]]))

(defn genkw [ns name]
  "Same as gensym but for namespaced keywords."
  (keyword (str ns) (str (gensym name))))

(defmacro def-lucene-syntax [query-spec-kw fields-set]
  "Macro generating specs for lucene-like syntax in clojure data structure given
  a set of search fields keywords and 'query' namespaced keyword used to name
  the generated spec.

  (def-lucene-syntax ::query #{:field1 :field2})
  (s/valid? ::query [:and \"foo\" [:field1 \"value1\"]])
  ; => true
  "
  (let [term-kw (genkw *ns* "term-kw")
        field-kw (genkw *ns* "field-kw")]
    `(do
        ; Lucene term
        ; Examples: "A", [:or "A" "B"], [:and "A" "B"]
       (s/def ~term-kw
         (s/or :str string?
               :operand (s/cat :operator #{:or :an}
                               :terms (s/+ ~term-kw))))

        ; Lucene field search ("field:value")
        ; Examples: [:field1 "A"], [:field2 [:or "B" "C"]]
       (s/def ~field-kw
         (s/cat :name ~fields-set
                :value ~term-kw))

        ; Lucene query
        ; Examples: "A", [:and "B" [:field1 "C"]], [:field2 "D"]
       (s/def ~query-spec-kw
         (s/or :term ~term-kw
                :field ~field-kw
                 ; resursivly nested sub-queries
                :operand (s/cat :operator #{:or :and}
                                :queries (s/+ ~query-spec-kw)))))))

(defn char-range-set [start end]
  "Generate a range set of characters between two given characters"
  (->> (range (int start) (inc (int end)))
       (map (comp #(get (str %) 0) char))
       (into #{})))

;[0-9]
(def digit-set
  (char-range-set \0 \9))

;[A-Z]
(def alpha-set
  (char-range-set \A \Z))

; [A-Z0-9]
(def digit-alpha-set
  (union digit-set alpha-set))

; [a-zA-Z0-9_]
(def word-set
  (union digit-set alpha-set (char-range-set \a \z) #{\_}))


(defn- s-repeat-size [spec size]
  (let [body (interleave
               (->> (range size) (map (comp keyword str)))
               (repeat size spec))]
    `(s/cat ~@body)))

(defn- s-repeat-range [spec start end]
  (let [rrange (range start end)
        s-repeat (partial s-repeat-size spec)
        body (interleave
               (->> rrange (map (comp keyword str)))
               (->> rrange (map (comp s-repeat inc))))]
    `(s/alt ~@body)))

(defmacro s-repeat
  "Example:
    (s-repeat alpha-set 2 4) ;equivalent to [A-Z]{2,4}"
  ([spec start end] (s-repeat-range spec start end))
  ([spec size] (s-repeat-size spec size)))
