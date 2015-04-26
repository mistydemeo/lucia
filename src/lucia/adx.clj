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

(defn- take-uint
  "Reads an unsigned short from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-uint bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFFFFFFFF (long (.getInt bytebuf index)))))

(defn- read-bytes
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a ByteBuffer of the specified endianness."
  ([f count byte-order]
    (read-bytes f 0 count byte-order))
  ([f offset count byte-order]
  (let [stream (io/input-stream f) bytes (make-array Byte/TYPE count) buf (ByteBuffer/allocate count)]
    (.order buf byte-order)
    (.skip stream offset)
    (.read stream bytes 0 count)
    (.put buf bytes))))

(defn- read-bytes-le
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a little-endian ByteBuffer."
  ([f count]
    (read-bytes-le f 0 count))
  ([f offset count]
    (read-bytes f offset count ByteOrder/LITTLE_ENDIAN)))

(defn- read-bytes-be
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a big-endian ByteBuffer."
  ([f count]
    (read-bytes-be f 0 count))
  ([f offset count]
    (read-bytes f offset count ByteOrder/BIG_ENDIAN)))

(defn header-valid?
  "Reads File `f`'s magic bytes to determine if it is a valid ADX file."
  [f]
  (let [magic-bytes (read-bytes-be f 2)]
    (= 0x8000 (take-ushort magic-bytes))))

(defn signature-valid?
  "Determines whether File `f` contains a valid ADX signature.
   The expected signature is the text \"(c)CRI\"."
  [f]
  (let [offset (+ 4 (take-ushort (read-bytes-be f 2 2)))]
    (and
      (= 0x2863 (take-ushort (read-bytes-be f (- offset 6) 2))) ; "(c"
      (= 0x29435249 (take-uint (read-bytes-be f (- offset 4) 4)))))) ; ")CRI"
