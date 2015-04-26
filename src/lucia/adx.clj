(ns lucia.adx
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder)))

; Based on a function from https://github.com/geoffsalmon/bytebuffer
(defn- take-ushort
  "Reads an unsigned short from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-ushort bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFFFF (long (.getShort bytebuf index)))))

(defn- read-bytes
  "Reads `count` bytes from File `f` and returns a ByteBuffer of the specified endianness."
  [f count byte-order]
  (let [stream (io/input-stream f) bytes (make-array Byte/TYPE count) buf (ByteBuffer/allocate count)]
    (.order buf byte-order)
    (.read stream bytes 0 count)
    (.put buf bytes)))

(defn- read-bytes-le
  "Reads `count` bytes from File `f` and returns a little-endian ByteBuffer."
  [f count]
  (read-bytes f count ByteOrder/LITTLE_ENDIAN))

(defn- read-bytes-be
  "Reads `count` bytes from File `f` and returns a big-endian ByteBuffer."
  [f count]
  (read-bytes f count ByteOrder/BIG_ENDIAN))

(defn header-valid?
  "Reads File `f`'s magic bytes to determine if it is a valid ADX file."
  [f]
  (let [magic-bytes (read-bytes-be f 2)]
    (= 0x8000 (take-ushort magic-bytes))))
