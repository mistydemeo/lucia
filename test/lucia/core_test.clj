(ns lucia.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [lucia.core :as core]))

(def mono-test-file
  (io/file (io/resource "flute-alto-lick-mono.adx")))

(deftest core-test
  (testing "it should return nil when attempting to identify an unknown file"
    (is (nil? (core/identify mono-test-file)))))
