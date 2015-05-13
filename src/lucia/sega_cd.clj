(ns lucia.sega-cd
  (:require [lucia.byte-tools :as byte-tools])
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

; Value provided by kode54; "determined from the PCM chip's base clock divided by the rate value the game uses"
; This is double the native value, 16282, because the empty 0-byte samples are not filtered out
(def frequency
  32564)

(defn read-frame
  "Given an InputStream, reads a 2048-byte sector and returns it as a Byte[].
   If the end of the file has been reached, returns nil instead."
  [stream]
  (let [frame (make-array Byte/TYPE 2048)]
    (if-not (= -1 (.read stream frame 0 2048))
      frame)))

(defn- interpret-channel-count
  "Interprets the number of channels in the file given the value from the header.
   Confusingly, \"1\" indicates that there are 2 channels, and vice versa."
  [count]
  (case count
    2 1
    1 2))

(defn- calculate-loop-start
  [header]
  "Extracts the loop start position from the header."
  (bit-shift-left (bit-or
    (bit-shift-left (nth header 2) 24)
    (bit-shift-left (nth header 3) 16)
    (bit-shift-left (nth header 4) 8)
    (nth header 5)) 11))

(defn- calculate-loop-end
  "Extracts the loop end position from the header."
  [header]
  (+ 1 (bit-or
    (bit-shift-left (nth header 6) 24)
    (bit-shift-left (nth header 7) 16)
    (bit-shift-left (nth header 8) 8)
    (nth header 9))))

(defn parse-header
  "Given the header of a PCM file, as a Byte[], return a map containing all of the associated metadata."
  [header]
  {
    :channel-count (interpret-channel-count (nth header 1))
    :loop-start (calculate-loop-start header)
    :loop-end (calculate-loop-end header)
    })

(defn- stereo?
  [header]
  (= (:channel-count header) 2))

(defn- interleave-stereo-frames
  "Given two Byte[] arrays, returns a new Byte[] array with the original arrays' values interleaved.
   This is useful when processing stereo data from Sega CD PCM files, as standard PCM interleaves every sample while Eternal Blue PCM files interleave frames."
  [left right]
  (into-array Byte/TYPE (interleave left right)))

(defn- amplify-byte
  [byt]
  (let [byt (bit-and byt 0xFF)]
    (if-not (= 0 (bit-and byt 0x80))
      (* 0x100 (- 0 (bit-and byt 0x7F)))
      (* 0x100 byt))))

(defn- amplify-frame
  [frame]
  (let [buffer (ByteBuffer/allocate (* 2 (alength frame)))
        amplified-frame (map amplify-byte frame)]
    (doseq [sample amplified-frame]
      (.putShort buffer sample))
    (.array buffer)))

(defn- read-and-process-frame
  [stream header]
  (if (stereo? header)
    (let [left (read-frame stream)
          right (read-frame stream)]
      (if-not (some nil? [left right]) ; EOF
        (apply interleave-stereo-frames [left right])))
    ; single mono frame    
    (read-frame stream)))

(defn decode-file
  [f output]
  (let [input (io/input-stream f)
        header-data (parse-header (read-frame input))]
    (loop []
      (let [decoded-frame (read-and-process-frame input header-data)]
        (if-not (nil? decoded-frame)
          (let [amplified-frame (amplify-frame decoded-frame)]
            (.write output amplified-frame 0 (alength amplified-frame))
            (recur)))))))
