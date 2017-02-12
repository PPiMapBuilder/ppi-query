(ns ppi-query.test.utils
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.template :as temp]))

(defmacro are-valid [spec & data-samples]
  "Assert that the given data samples conforms to the given clojure spec"
  (let [value-sym (gensym "value")]
    `(temp/do-template [~value-sym]
      (is
        ; Value should conform to spec
        (s/valid? ~spec ~value-sym)
        ; If not: explain
        (s/explain-str ~spec ~value-sym))
      ~@data-samples)))

(defmacro are-invalid [spec & data-samples]
  "Assert that the given data samples does not conforms to the given clojure spec"
  (let [value-sym (gensym "value")]
    `(temp/do-template [~value-sym]
      (is
        ; Value should conform to spec
        (not (s/valid? ~spec ~value-sym))
        ; If not: explain
        (s/conform ~spec ~value-sym))
      ~@data-samples)))

;; Utility functions to intergrate clojure.spec.test/check with clojure.test
(defn summarize-results' [spec-check]
  "Pretty print abbreviated results for clojure.spec function check."
  (map
    (comp #(pprint/write % :stream nil) stest/abbrev-result)
    spec-check))

(def trace-i (atom 0))
(defn trace [x]
  (println "trace" @trace-i x)
  (swap! trace-i inc)
  x)

(defn check [spec]
  "Check function spec as clojure.test condition"
  (let [spec-check (stest/check spec)]
    (is (-> spec-check first :failure nil?)
        (summarize-results' spec-check))))
