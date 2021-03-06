(ns lucia.sega-cd
  (:require [lucia.byte-tools :as byte-tools]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.io RandomAccessFile)))

; Value provided by kode54; "determined from the PCM chip's base clock divided by the rate value the game uses"
; This is double the native value, 16282, because the empty 0-byte samples are not filtered out
(def frequency
  32564)

; Size of a frame, and also (not coincidentally) the size of a mode 1 sector on a CD-ROM.
(def sector-size
  2048)

(defn read-frame
  "Given an InputStream, reads a 2048-byte sector and returns it as a Byte[].
   If the end of the file has been reached, returns nil instead."
  [stream]
  (let [frame (make-array Byte/TYPE sector-size)]
    (if-not (= -1 (.read stream frame 0 sector-size))
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
  (let [header (ByteBuffer/wrap header)]
    (bit-shift-left (bit-or
      (bit-shift-left (byte-tools/take-ubyte header 2) 24)
      (bit-shift-left (byte-tools/take-ubyte header 3) 16)
      (bit-shift-left (byte-tools/take-ubyte header 4) 8)
      (byte-tools/take-ubyte header 5)) 11)))

(defn- calculate-loop-end
  "Extracts the loop end position from the header."
  [header]
  (let [header (ByteBuffer/wrap header)]
    ; The native value is multiplied by two, because the native value is
    ; in *samples*, whereas we're looking for values in *bytes*.
    ; Note that this is different from loop start, which is in fact
    ; natively stored as bytes. 
    (* 2 (+ 1 (bit-or
      (bit-shift-left (byte-tools/take-ubyte header 6) 24)
      (bit-shift-left (byte-tools/take-ubyte header 7) 16)
      (bit-shift-left (byte-tools/take-ubyte header 8) 8)
      (byte-tools/take-ubyte header 9))))))

(defn parse-header
  "Given the header of a PCM file, as a Byte[], return a map containing all of the associated metadata."
  [header]
  (let [channel-count (interpret-channel-count (nth header 1))
        loop-start (calculate-loop-start header)
        loop-end (calculate-loop-end header)]
    {:channel-count channel-count
     ; offsets on all of these values are from the start of *content*, not
     ; the start of the *file*; add 2048 to account for the header 
     :loop-start (+ sector-size loop-start)
     ; loop-end is treated internally as samples, with left-right pairs being
     ; considered to be a single sample;
     ; for stereo audio, multiply this by two to get a true byte count
     :loop-end (if (= 2 channel-count) (* loop-end 2) loop-end)}))

(defn- stereo?
  [header]
  (= (:channel-count header) 2))

(defn- interleave-stereo-frames
  "Given two Byte[] arrays, returns a new Byte[] array with the original arrays' values interleaved.
   This is useful when processing stereo data from Sega CD PCM files, as standard PCM interleaves every sample while Eternal Blue PCM files interleave frames."
  [left right]
  (into-array Byte/TYPE (interleave left right)))

(defn- convert-byte-s8-to-s16
  [byt]
  (let [byt (bit-and byt 0xFF)]
    (if-not (= 0 (bit-and byt 0x80))
      (* 0x100 (- 0 (bit-and byt 0x7F)))
      (* 0x100 byt))))

(defn- convert-frame-s8-to-s16
  [frame]
  (let [buffer (ByteBuffer/allocate (* 2 (alength frame)))
        s16-frame (map convert-byte-s8-to-s16 frame)]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (doseq [sample s16-frame]
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
  ([f output]
    (decode-file f output 2))
  ([f output loops]
  (let [input (new RandomAccessFile f "r")
        header-data (parse-header (read-frame input))]
    (loop [loop-count 1
           bytes-played 0]
      (let [decoded-frame (read-and-process-frame input header-data)]
        (if-not (nil? decoded-frame)
          (let [s16-frame (convert-frame-s8-to-s16 decoded-frame)
                current-frame-size (alength decoded-frame)
                delta (- (:loop-end header-data) bytes-played)
                loop-done? (< delta current-frame-size)
                looping-done? (= loops loop-count)
                new-loop-count (if loop-done? (inc loop-count) loop-count)
                new-bytes-played (if loop-done?
                  ; bytes-played resets back to the duration of the loop if loop is over
                  (:loop-start header-data)
                  (+ bytes-played current-frame-size))]
            (if (and (not looping-done?) loop-done?)
              (do
                (.seek input (:loop-start header-data))
                ; write double delta because delta
                ; represents the size of the original 8-bit samples;
                ; the actual block being written is double this size
                (.write output s16-frame 0 (* 2 delta)))
              ; if all loops have been played, just continue playing to end of file
              (.write output s16-frame 0 (alength s16-frame)))
            (recur new-loop-count new-bytes-played))))))))
