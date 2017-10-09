(ns ppi-query.spec-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]
            [ppi-query.spec :as ps]
            [clojure.spec.alpha :as s]
            [ppi-query.xml :as pxml]
            [clojure.spec.test.alpha :as stest]))

(deftest* test-spec
  ; Generate lucene-like query syntax specs with two possible field search
  (ps/def-lucene-syntax ::query #{:field1 :field2})

  (testing "Check query spec with a list of valid and invalid examples"
    (are-spec ::query
      :valid [; single term
              "A"
              ; term combination
              [:and "A"], [:or "A" "B"]
              ; single field
              [:field1 "A"]
              ; fields and terms combination
              [:and [:field2 "A"] "B"], [:field1 [:or "A" "B"]]
              ; nested queries of fields and terms
              [:and [:field1 [:or "A" "B"]] "C" [:and "D" [:field2 "E"]]]]

      :invalid [; nil, empty seq or keywords
                nil, [], :A
                ; multiple field value without specifying an
                ; operand ("or" or "and")
                [:field1 "A" "B"]
                ; invalid field name
                [:foo "A"]]))

  (testing "Fix size spec repeat"
    (are-spec (ps/s-repeat int? 10)

      :valid [(range 10)
              [1 2 2 4 1 4 5 5 2 4]]

      :invalid [(repeat 10 nil)
                (range 3)]))

  (testing "Range size spec repeat"
    (are-spec (ps/s-repeat int? 5 10)

      :valid [(range 10)
              (range 5)
              (repeatedly 7 #(rand-int 99))]

      :invalid [(repeat 6 nil)
                (range 3)
                (range 11)]))

  (testing "Map spec"
    (are-spec (ps/map-spec :a int? :b #{:c :d} :e nil?)

      :valid [{:a 1 :b :c :e nil :f nil :g 12}
              {:e nil :b :d :a 20}
              {:b :d :e nil :a 10}]

      :invalid [{:a 1}
                {:a 1 :b :c}
                {:a nil :b nil :e nil}]))

  (testing "XML map spec"

    (is (s/valid? (pxml/node :tag #{:a} :attrs (s/map-of keyword? string?))
                  {:tag :a :attrs {:foo "bar"} :content nil}))

    (is (s/valid? (pxml/node :tag #{:a :b} :content (s/tuple string?))
                  {:tag :b :attrs nil :content [""]}))))

