(ns ppi-query.core-test
  (:require [clojure.spec.test :as stest]
            [ppi-query.test.utils :refer :all]))

;; Activate clojure.spec.test instrument of all spec'ed functions
(stest/instrument)

(defspec-test test-utils
  [`ppi-query.utils/merge-distinct])
