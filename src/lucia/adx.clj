(ns lucia.adx
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder)))

; Based on functions from https://github.com/geoffsalmon/bytebuffer
(defn- take-ubyte
  "Reads a single unsigned byte from ByteBuffer `bytebuf` and returns it as a Long."
  ([bytebuf]
  (take-ubyte bytebuf 0))
  ([bytebuf index]
  (bit-and 0xFF (long (.get bytebuf index)))))

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

(defn get-stream-offset
  "Returns the offset from the beginning of the file at which the stream content begins.
   The stream offset is located immediately after the (c)CRI signature."
  [f]
  (+ 4 (take-ushort (read-bytes-be f 2 2))))

(defn header-valid?
  "Reads File `f`'s magic bytes to determine if it is a valid ADX file."
  [f]
  (let [magic-bytes (read-bytes-be f 2)]
    (= 0x8000 (take-ushort magic-bytes))))

(defn signature-valid?
  "Determines whether File `f` contains a valid ADX signature.
   The expected signature is the text \"(c)CRI\"."
  [f]
  (let [offset (get-stream-offset f)]
    (and
      (= 0x2863 (take-ushort (read-bytes-be f (- offset 6) 2))) ; "(c"
      (= 0x29435249 (take-uint (read-bytes-be f (- offset 4) 4)))))) ; ")CRI"

(defn encoding-type
  "Returns the encoding type of the ADX file.
  Normal values are:
  * 2  (unknown)
  * 3  ADX
  * 4  ADX (exponential scale)
  * 17 AHX"
  [f]
  (take-ubyte (read-bytes-be f 4 1)))

(defn frame-size
  "Returns the frame size of the ADX file.
  Values other than 18 are rare (non-extant?) in practice."
  [f]
  (take-ubyte (read-bytes-be f 5 1)))

(defn version
  "Returns the version of the ADX file.
  Values include:
  * 0x0300 - ADX 3
  * 0x0400 - ADX 4
  * 0x0408 - Encrypted ADX 8
  * 0x0409 - Encrpyted ADX 9
  * 0x0500 - Simple, loopless ADX found as the audio stream in some Sofdec video; vgmstream cites Buggy Heat as an example

  Note that lucia/adx currently only supports 0x0300 ADX files."
  [f]
  (take-ushort (read-bytes-be f 0x12 2)))

(defn- loop-info-3
  "Returns loop information for 0x0300 ADX files."
  [f]
  (let [offset (get-stream-offset f)]
    (if (>= (- offset 6) 0x2c) ; check for enough room in the header for loop data
      [
        (take-uint (read-bytes-be f 0x18 4)) ; loop flag
        (take-uint (read-bytes-be f 0x1c 4)) ; sample at which loop starts
        (take-uint (read-bytes-be f 0x24 4)) ; sample at which loop ends
      ]
      ; default
      [0 0 0])))

(defn loop-info
  "Returns loop information, as a vector of [loop_flag, loop_start, loop_end].
  If the file contains no loop data, or is in an unsupported ADX format, returns [0 0 0]."
  [f]
  (case (version f)
    0x0300 (loop-info-3 f)
    [0 0 0]))

(defn cutoff
  "Returns the cutoff frequency for File `f`, as a Long.
  MultimediaWiki says this is always 500Hz in practice."
  [f]
  (take-ushort (read-bytes-be f 0x10 2)))

(defn channels
  "Returns the number of the channels in File `f`, usually 1 (mono) or 2 (stereo)."
  [f]
  (take-ubyte (read-bytes-be f 7 1)))

(defn sample-rate
  "Returns the sample rate of File `f`."
  [f]
  (take-uint (read-bytes-be f 8 4)))

(defn sample-count
  "Returns the number of samples in File `f`."
  [f]
  (take-uint (read-bytes-be f 0xc 4)))

; TODO make this overload on input type, so a File can be passed in
(defn calculate-coefficients
  "Calculates the linear predictor coefficients given the cutoff frequency and the sample rate.
  Returns a vector of [coefficient_1 coefficient_2], both values as signed integers."
  [cutoff-filter sample-rate]
  (let [sqrt2 (Math/sqrt 2) z (Math/cos (/ (* (* 2 Math/PI) cutoff-filter) sample-rate))
        a (- sqrt2 z) b (- sqrt2 1) c (/ (- a (Math/sqrt (* (+ a b) (- a b)))) b)]
        [(int (Math/floor (* c 8192)))
         (int (Math/floor (* c (* c -4096))))]))
