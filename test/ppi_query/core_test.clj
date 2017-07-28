(ns ppi-query.core-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]))

(deftest test-utils
  (check' `ppi-query.utils/merge-distinct))
