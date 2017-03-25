(ns ppi-query.test.utils
  (:require [clojure.test :as t]
            [clojure.pprint :as pprint]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.template :as temp]))

(defmacro are-valid [spec & data-samples]
  "Assert that the given data samples conforms to the given clojure spec."
  (let [value-sym (gensym "value")]
    `(temp/do-template [~value-sym]
      (t/is
        ; Value should conform to spec
        (s/valid? ~spec ~value-sym)
        ; If not: explain
        (s/explain-str ~spec ~value-sym))
      ~@data-samples)))

(defmacro are-invalid [spec & data-samples]
  "Assert that the given data samples does not conforms to the given
   clojure spec."
  (let [value-sym (gensym "value")]
    `(temp/do-template [~value-sym]
      (t/is
        ; Value should conform to spec
        (not (s/valid? ~spec ~value-sym))
        ; If not: explain
        (s/conform ~spec ~value-sym))
      ~@data-samples)))

(def trace-i (atom 0))
(defn trace [x]
  (println "trace" @trace-i x)
  (swap! trace-i inc)
  x)

(defn- spec-test [sym-or-syms opts]
  "Helper function for defspec-test."
  `(fn []
    (let [check-results# (stest/check ~sym-or-syms ~opts)
          checks-passed?# (every? nil? (map :failure check-results#))]
      (if checks-passed?#
        (t/do-report {:type    :pass
                      :message (->> (map :sym check-results#)
                                    (clojure.string/join ", ")
                                    (str "Generative tests pass for "))})
        (doseq [failed-check# (filter :failure check-results#)
                :let [r# (stest/abbrev-result failed-check#)
                      failure# (:failure r#)]]
          (t/do-report
            {:type     :fail
             :message  (with-out-str (s/explain-out failure#))
             :expected (->> r# :spec rest (apply hash-map) :ret)
             :actual   (if (instance? Throwable failure#)
                         failure#
                         (::stest/val failure#))})))
      checks-passed?#)))

;; https://gist.github.com/kennyjwilli/8bf30478b8a2762d2d09baabc17e2f10
(defmacro defspec-test
  "Define a clojure.test test using clojure.spec generative testing of spec'ed
   functions.

   Example: (defspec-test test-name [namespace/function1
                                     namespace/function2])"
  ([name sym-or-syms] `(defspec-test ~name ~sym-or-syms nil))
  ([name sym-or-syms opts]
   (when t/*load-tests*
     `(def ~(vary-meta name assoc :test (spec-test sym-or-syms opts))
        (fn [] (t/test-var (var ~name)))))))
