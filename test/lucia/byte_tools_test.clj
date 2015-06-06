(ns lucia.byte-tools-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [lucia.byte-tools :as byte-tools])
  (:import (java.nio ByteBuffer)))

; First byte is signed to test handling of unsigned values
(def buf
  (ByteBuffer/wrap (byte-array [-127 0 0 0])))

(def mono-test-file
  (io/file (io/resource "flute-alto-lick-mono.adx")))

(deftest byte-tools-test
  (testing "it should be able to extract signed nibbles from unsigned bytes"
    (is (= -1 (byte-tools/get-low-nibble 255)))
    (is (= -1 (byte-tools/get-high-nibble 255)))
    (is (= 0 (byte-tools/get-low-nibble 0)))
    (is (= 0 (byte-tools/get-high-nibble 0))))

  (testing "it should be able to clamp bytes to the 16-bit signed range"
    (is (= 0 (byte-tools/clamp16 0)))
    (is (= -32767 (byte-tools/clamp16 -65535)))
    (is (= 32767 (byte-tools/clamp16 32768))))

  (testing "it should be able to read an unsigned byte from a ByteBuffer"
    (is (= 0x81 (byte-tools/take-ubyte buf 0)))
    (is (= 0 (byte-tools/take-ubyte buf 1)))
    (is (= 0 (byte-tools/take-ubyte buf 2)))
    (is (= 0 (byte-tools/take-ubyte buf 3))))

  (testing "it should be able to read an unsigned short from a ByteBuffer"
    (is (= 0x8100 (byte-tools/take-ushort buf 0)))
    (is (= 0 (byte-tools/take-ushort buf 2))))

  (testing "it should be able to read an unsigned int from a ByteBuffer"
    (is (= 0x81000000 (byte-tools/take-uint buf 0))))

  (testing "it should be able to read bytes from a file"
    (is (= '(-0x80 0x00 0x00 0x20) (seq (.array (byte-tools/read-bytes-le mono-test-file 0 4)))))
    (is (= '(-0x80 0x00 0x00 0x20) (seq (.array (byte-tools/read-bytes-be mono-test-file 0 4)))))))
