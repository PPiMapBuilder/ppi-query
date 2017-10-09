(ns ppi-query.core-test
  (:require [clojure.test :refer :all]
            [ppi-query.test.utils :refer :all]
            [clojure.spec.test.alpha :as stest]))

(deftest* test-utils

  (testing "Test check merge distinct"
    (check' `ppi-query.utils/merge-distinct)))

