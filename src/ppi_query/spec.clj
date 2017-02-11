(ns ppi-query.spec
  (:require [clojure.spec :as s]))

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
