(ns ppi-query.test.utils
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
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
