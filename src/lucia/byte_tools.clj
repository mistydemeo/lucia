(ns lucia.byte-tools
  "Provides helper functions for reading values from File objects."
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder)))

; Based on functions from https://github.com/geoffsalmon/bytebuffer
(defn take-ubyte
  "Reads a single unsigned byte from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-ubyte bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFF (long (.get bytebuf index)))))

(defn take-ushort
  "Reads an unsigned short from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-ushort bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFFFF (long (.getShort bytebuf index)))))

(defn take-uint
  "Reads an unsigned short from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-uint bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFFFFFFFF (long (.getInt bytebuf index)))))

(defn get-low-nibble
  "Given an eight-bit unsigned byte, returns the low nibble as a Long."
  [byt]
  (- (bit-and byt 7) (bit-and byt 8)))

(defn get-high-nibble
  [byt]
  (bit-shift-right (- (bit-and byt 0x70) (bit-and byt 0x80)) 4))

(defn clamp16
  [i]
  (cond
    (> i 32767) 32767
    (< i -32767) -32767
    :else i))

(defn read-bytes
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a ByteBuffer of the specified endianness."
  ([f count byte-order]
    (read-bytes f 0 count byte-order))
  ([f offset count byte-order]
  (let [stream (io/input-stream f) bytes (make-array Byte/TYPE count) buf (ByteBuffer/allocate count)]
    (.order buf byte-order)
    (.skip stream offset)
    (.read stream bytes 0 count)
    (.put buf bytes))))

(defn read-bytes-le
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a little-endian ByteBuffer."
  ([f count]
    (read-bytes-le f 0 count))
  ([f offset count]
    (read-bytes f offset count ByteOrder/LITTLE_ENDIAN)))

(defn read-bytes-be
  "Reads `count` bytes from File `f` (beginning at `offset` or 0) and returns a big-endian ByteBuffer."
  ([f count]
    (read-bytes-be f 0 count))
  ([f offset count]
    (read-bytes f offset count ByteOrder/BIG_ENDIAN)))
