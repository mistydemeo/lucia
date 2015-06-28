(ns lucia.helpers-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [lucia.helpers :as helpers]))

(def mono-test-file
  (io/file (io/resource "flute-alto-lick-mono.adx")))

(deftest helpers-test
  (testing "it should return nil when attempting to identify an unknown file"
    (is (nil? (helpers/identify mono-test-file)))))
