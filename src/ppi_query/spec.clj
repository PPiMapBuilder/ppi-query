(ns ppi-query.spec
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.set :refer [union]]
            [ppi-query.xml :as pxml]))

(defn gen-namespaced-kw [name]
  "Generate a namespaced keyword with a random namespace."
  (keyword (str (gensym)) name))

(s/fdef gen-namespaced-kw
  :args (s/cat :name string?)
  :ret keyword?)

(defmacro def-lucene-syntax [query-spec-kw fields-set]
  "Macro generating specs for lucene-like syntax in clojure data structure given
   a set of search fields keywords and 'query' namespaced keyword used to name
   the generated spec.

   (def-lucene-syntax ::query #{:field1 :field2})
   (s/valid? ::query [:and \"foo\" [:field1 \"value1\"]])
   ; => true"
  (let [term-kw (gen-namespaced-kw "term-kw")
        field-kw (gen-namespaced-kw "field-kw")]
    `(do
       ; Lucene term
       ; Examples: "A", [:or "A" "B"], [:and "A" "B"]
       (s/def ~term-kw
         (s/or :value (s/or :str string? :num number?)
               :operand (s/cat :operator #{:or :and}
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

(s/fdef char-range-set
  :args (s/and (s/cat :start char? :end char?)
               #(< (-> % :start int) (-> % :end int)))
  :ret (s/coll-of char? :distinct true)
  :fn #(some #{(-> % :args :start) (-> % :args :end)} (:ret %)))

; Construct usefull regexp-like character groups
(let [digit-set           (char-range-set \0 \9)
      maj-alpha-set       (char-range-set \A \Z)
      min-alpha-set       (char-range-set \a \z)
      digit-maj-alpha-set (union digit-set maj-alpha-set)
      word-set            (union digit-maj-alpha-set min-alpha-set #{\_})]
  ; [0-9] or \d
  (s/def ::digit-char digit-set)
  ; \D
  (s/def ::non-digit-char
         (s/and char?
                (comp not digit-set)))
  ; [A-Z]
  (s/def ::maj-alpha-char maj-alpha-set)
  ; [a-z]
  (s/def ::min-alpha-char min-alpha-set)
  ; [A-Z0-9]
  (s/def ::digit-maj-alpha-char digit-maj-alpha-set)
  ; [a-zA-Z0-9_] or \w
  (s/def ::word-char word-set))

(defn- s-repeat-size [spec size]
  "Helper function for the s-repeat macro."
  (let [body (interleave
               (->> (range size) (map (comp keyword str)))
               (repeat size spec))]
    `(s/cat ~@body)))

(s/fdef s-repeat-size
  :args (s/cat :spec any? :size pos-int?)
  :ret (s/cat :cat #{`s/cat}
              :parts (s/+ (s/cat :kw keyword?
                                 :spec any?))))

(defn- s-repeat-range [spec start end]
  "Helper function for the s-repeat macro."
  (let [rrange (range start (inc end))
        s-repeat (partial s-repeat-size spec)
        body (interleave
               (->> rrange (map (comp keyword str)))
               (->> rrange (map s-repeat)))]
    `(s/alt ~@body)))

(s/fdef s-repeat-range
  :args (s/and (s/cat :spec any? :start pos-int? :end pos-int?)
               #(< (:start %) (:end %)))
  :ret (s/cat :cat #{`s/alt}
              :parts (s/+ (s/cat :kw keyword?
                                 :spec any?))))
(defmacro s-repeat
  "Creates a sequence spec restricting the repetition of a given spec either
   a defined amount or a bounded amount.

   Example:
    (s-repeat int? 3) ; sequence of 3 integers
    (s-repeat ::maj-alpha-char 2) ;equivalent to [A-Z]{2}
    (s-repeat ::maj-alpha-char 2 4) ;equivalent to [A-Z]{2,4}"
  ([spec start end] (s-repeat-range spec start end))
  ([spec size] (s-repeat-size spec size)))

(s/def ::char-seq (s/coll-of char?))

(defn conform-to-str [x]
  (cond
    (string? x) x
    (seqable? x) (apply str (seq x))
    :else (str x)))

(defn conform-to-integer [x]
  (cond
    (integer? x) x
    (string? x) (Long/parseLong x)
    (s/valid? ::char-seq x) (recur (conform-to-str x))
    :else ::s/invalid))

(defn conform-to-double [x]
  (cond
    (double? x) x
    (string? x) (Double/parseDouble x)
    (s/valid? ::char-seq x) (recur (conform-to-str x))
    :else ::s/invalid))

(defn conform-to-char-seq [x]
  (cond
    (s/valid? ::char-seq x) x
    (string? x) (seq x)
    :else (recur (conform-to-str x))))

(defn conformer-spec [spec conf unf]
  "Creates a new spec from a spec, a conformer and a unformer attaching them
   with a custom generator."
  (s/with-gen
    (s/and
      (s/conformer conf unf)
      spec)
    #(gen/fmap unf (s/gen spec))))

(defn string-spec [seq-spec]
  "Creates a string spec from a character sequence spec using conformers and
   custom generators."
  (conformer-spec seq-spec
                  conform-to-char-seq
                  conform-to-str))

(defmacro map-spec [& entries]
  "Creates a spec describing hash-map entries.

   Example:
     (s/exercise (map-spec :a int? :b keyword?))
     => {:a -1, :b :-} ..."
  (let [s-def (fn [[kw spec]] `(s/def ~(gen-namespaced-kw (name kw))
                                      ~spec))
        defs (->> entries (partition 2) (map s-def))]
    `(do ~@defs
         (s/keys :req-un ~(map second defs)))))

(defn xml-spec [& {:keys [tag attrs content]}]
  "Creates an xml node spec describind its tag, attibutes and content."
  (s/and
    (map-spec :tag (if (keyword? tag) (hash-set tag) tag)
              :attrs (if (nil? attrs) nil? attrs)
              :content (if (nil? content) nil? content))
    ::pxml/node))
