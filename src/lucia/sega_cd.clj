(ns lucia.sega-cd
  (:require [lucia.byte-tools :as byte-tools])
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

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

(defn- filter-frame
  "Given a Byte[] or other sequence containing a raw frame read from a PCM file, returns a version with only the used (even) bytes as a seq.
   Eternal Blue's frames are 2048 bytes long, but contain only 1024 bytes of data (as 8-bit signed integers); every other byte is empty."
  [frame]
  (into-array Byte/TYPE (keep-indexed #(if (odd? %1) %2) frame)))

(defn- interleave-stereo-frames
  "Given two Byte[] arrays, returns a new Byte[] array with the original arrays' values interleaved.
   This is useful when processing stereo data from Sega CD PCM files, as standard PCM interleaves every sample while Eternal Blue PCM files interleave frames."
  [left right]
  (into-array Byte/TYPE (interleave left right)))

(defn- read-and-process-frame
  [stream header]
  (if (stereo? header)
    (let [left (read-frame stream)
          right (read-frame stream)]
      (if-not (some nil? [left right]) ; EOF
        (apply interleave-stereo-frames (map filter-frame [left right]))))
    (let [frame (read-frame stream)]
      (if-not (nil? frame) ; EOF
        (filter-frame frame)))))

(defn decode-file
  [f output]
  (let [input (io/input-stream f)
        header-data (parse-header (read-frame input))]
    (loop []
      (let [decoded-frame (read-and-process-frame input header-data)]
        (if-not (nil? decoded-frame)
          (do 
            (.write output decoded-frame 0 (alength decoded-frame))
            (recur)))))))
