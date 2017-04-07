(ns ppi-query.test.utils
  (:require [clojure.test :as t]
            [clojure.pprint :as pprint]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.template :as temp]
            [aprint.core :refer :all]))

(defmacro are-spec [spec & {:keys [valid invalid]}]
  "Assert that the given data samples conforms to the given clojure spec."
  (let [value-sym (gensym "value")]
    `(do
      (when ~valid
        (temp/do-template [~value-sym]
          (t/is
           ; Value should conform to spec
           (s/valid? ~spec ~value-sym)
           ; If not: explain why it doesn't conform
           (->> (s/explain-data ~spec ~value-sym)
                (:clojure.spec/problems)
                (aprint) ; pretty print with colors!
                (with-out-str)
                (str "\nNot conforming:\n")))
          ~@valid))
      (when ~invalid
        (temp/do-template [~value-sym]
          (t/is
           ; Value should conform to spec
           (not (s/valid? ~spec ~value-sym))
           ; If not: explain how it conforms
           (->> (s/conform ~spec ~value-sym)
                (aprint) ; pretty print with colors!
                (with-out-str)
                (str "\nConforming:\n")))
          ~@invalid)))))

(def trace-i (atom 0))
(defn trace [x]
  (println "trace" @trace-i x)
  (swap! trace-i inc)
  x)

;;; Utility functions combining clojure spec test check and clojure test
;;; inspired by: https://gist.github.com/Risto-Stevcev/dc628109abd840c7553de1c5d7d55608

; Summarize exceptions
(def summarize-exception
  (comp #(apply str %)
        (juxt
          (constantly "\n")
          (comp #(str "Error type:    " %) class :failure)
          (constantly "\n")
          (comp #(str "Error message: " %) (memfn getMessage) :failure)
          (constantly "\n"))))

; Summarize failures
(def summarize-failure
  (comp #(apply str %)
        (juxt
          (constantly "\n")
          (comp #(str "Input args:\t" %) #(into [] %) :clojure.spec.test/args :failure)
          (constantly "\n")
          (comp #(str "Ouput ret:\t" %) :clojure.spec.test/val :failure)
          (constantly "\n")
          (comp #(str "Failure:\t" %) :clojure.spec/failure :failure)
          (constantly "\n")
          (constantly "\n")
          (constantly "Problems:\n")
          ; Pretty print problems
          #(-> % :failure :clojure.spec/problems
               (aprint)
               (with-out-str)))))

(defn summarize-result [spec-check-result]
  (str
    "\n"
    "[ " (:sym spec-check-result) " ]"
    (if (-> spec-check-result :failure :clojure.spec/failure)
      (summarize-failure spec-check-result)
      (summarize-exception spec-check-result))))

;; Utility functions to intergrate clojure.spec.test/check with clojure.test
(defn summarize-results' [spec-check]
  "Generate summary of spec test check results"
  (->> spec-check
       (map (comp summarize-result stest/abbrev-result))
       (apply str)))

(defn succeded? [results]
  "Check if the clojure spec check test results are sucessful"
  (nil? (-> results first :failure)))

(defn check' [fn]
  "Combine clojure.test/is and clojure.spec.test/check"
  (stest/instrument fn)
  (let [spec-check (stest/check fn)
        spec-check-successful? (succeded? spec-check)]
    (t/is spec-check-successful?
          (when-not spec-check-successful?
            (summarize-results' spec-check)))))
