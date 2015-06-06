(ns lucia.adx-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [lucia.byte-tools :as byte-tools]
            [lucia.adx :as adx]))

(def mono-test-file
  (io/file (io/resource "flute-alto-lick-mono.adx")))

(def stereo-test-file
  (io/file (io/resource "flute-alto-lick-stereo.adx")))

(deftest adx-test
  (testing "it should be able to validate the file"
    (is (true? (adx/header-valid? mono-test-file)))
    (is (true? (adx/signature-valid? mono-test-file))))

  ; TODO obtain samples of more encoding types
  (testing "it should be able to determine the encoding type"
    (is (= 3 (adx/get-encoding-type mono-test-file))))

  ; All of the open-source encoders produce 0x0300 files;
  ; it would be good to obtain sample files of other ADX versions.
  (testing "it should be able to determine the ADX version"
    (is (= 0x0300 (adx/get-version mono-test-file))))

  ; TODO obtain samples containing loop information;
  ; ffmpeg can only produce loopless files.
  (testing "it should return default loop info for files without loops"
    (is (=
      {:has-loop false :loop-start 0 :loop-end 0}
      (adx/get-loop-info mono-test-file))))

  (testing "it should be able to determine the stream offset"
    (is (= 36 (adx/get-stream-offset mono-test-file))))

  (testing "it should be able to read the sample count"
    (is (= 135584 (adx/get-sample-count mono-test-file)))
    (is (= 135584 (adx/get-sample-count stereo-test-file))))

  (testing "it should be able to read the sampling rate"
    (is (= 44100 (adx/get-sample-rate mono-test-file))))

  ; I'm unaware of any files with non-18 frame sizes with which to test
  (testing "it should be able to determine the frame size"
    (is (= 18 (adx/get-frame-size mono-test-file))))

  ; Similarly, I'm unaware of any ADX files with a non-500Hz cutoff filter
  (testing "it should be able to determine the cutoff filter"
    (is (= 500 (adx/get-cutoff mono-test-file))))

  (testing "it should be able to return the number of audio channels"
    (is (= 1 (adx/get-channel-count mono-test-file)))
    (is (= 2 (adx/get-channel-count stereo-test-file))))

  (testing "it should be able to calculate the linear predictor coefficients given the cutoff filter and sample rate"
    (is (= [7334 -3284] (adx/calculate-coefficients (adx/get-cutoff mono-test-file) (adx/get-sample-rate mono-test-file)))))

  (testing "it should be able to decode a frame into a vector of samples"
    (let [frame (byte-tools/read-bytes-be mono-test-file 36 18)
          [decoded-frame hist] (adx/read-samples-from-frame frame 0 0 7334 -3284)]
      (is (=
        [45 -25 -66 -99 -95 -76 -120 -184 -264 -311 -331 -329 -279 -251 -241 -246 -278 -316 -343 -361 -312 -300 -378 -452 -522 -558 -491 -432 -425 -460 -453 -338]
        decoded-frame))
      (is (= (reverse hist) (last (partition 2 decoded-frame)))))))
