(ns lucia.adx
  "Provides functions to identify, read metadata from, and decode CRI ADX ADPCM files.
   The [MultimediaWiki](http://wiki.multimedia.cx/index.php?title=CRI_ADX_ADPCM), [vgmstream](http://hcs64.com/vgmstream.html) and [FFmpeg](http://ffmpeg.org/) were used as references."
  (:require [lucia.byte-tools :as byte-tools])
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer))
  (:import (java.io RandomAccessFile)))

(defn get-stream-offset
  "Returns the offset from the beginning of the file at which the stream content begins.
   The stream offset is located immediately after the (c)CRI signature."
  [f]
  (+ 4 (byte-tools/take-ushort (byte-tools/read-bytes-be f 2 2))))

(defn header-valid?
  "Reads File `f`'s magic bytes to determine if it is a valid ADX file."
  [f]
  (let [magic-bytes (byte-tools/read-bytes-be f 2)]
    (= 0x8000 (byte-tools/take-ushort magic-bytes))))

(defn signature-valid?
  "Determines whether File `f` contains a valid ADX signature.
   The expected signature is the text \"(c)CRI\"."
  [f]
  (let [offset (get-stream-offset f)]
    (and
      (= 0x2863 (byte-tools/take-ushort (byte-tools/read-bytes-be f (- offset 6) 2))) ; "(c"
      (= 0x29435249 (byte-tools/take-uint (byte-tools/read-bytes-be f (- offset 4) 4)))))) ; ")CRI"

(defn get-encoding-type
  "Returns the encoding type of the ADX file.
  Normal values are:
  * 2  (unknown)
  * 3  ADX
  * 4  ADX (exponential scale)
  * 17 AHX"
  [f]
  (byte-tools/take-ubyte (byte-tools/read-bytes-be f 4 1)))

(defn get-frame-size
  "Returns the frame size of the ADX file.
  Values other than 18 are rare (non-extant?) in practice."
  [f]
  (byte-tools/take-ubyte (byte-tools/read-bytes-be f 5 1)))

(defn get-version
  "Returns the version of the ADX file.
  Values include:
  * 0x0300 - ADX 3
  * 0x0400 - ADX 4
  * 0x0408 - Encrypted ADX 8
  * 0x0409 - Encrpyted ADX 9
  * 0x0500 - Simple, loopless ADX found as the audio stream in some Sofdec video; vgmstream cites Buggy Heat as an example

  Note that lucia.adx currently only supports 0x0300 ADX files."
  [f]
  (byte-tools/take-ushort (byte-tools/read-bytes-be f 0x12 2)))

(defn- get-loop-info-3
  "Returns loop information for 0x0300 ADX files."
  [f]
  (let [offset (get-stream-offset f)]
    (if (>= (- offset 6) 0x2c) ; check for enough room in the header for loop data
      {
        :has-loop (= 1 (byte-tools/take-uint (byte-tools/read-bytes-be f 0x18 4)))
        :loop-start (byte-tools/take-uint (byte-tools/read-bytes-be f 0x20 4))
        :loop-end (byte-tools/take-uint (byte-tools/read-bytes-be f 0x28 4))

      }
      ; default
      {
        :has-loop false
        :loop-start 0
        :loop-end 0
      })))

(defn- get-loop-info-4
  "Returns loop information for 0x0400 ADX files."
  [f]
  (let [offset (get-stream-offset f)]
    (if (>= (- offset 6) 0x2c) ; check for enough room in the header for loop data
      {
        :has-loop (= 1 (byte-tools/take-uint (byte-tools/read-bytes-be f 0x24 4)))
        :loop-start (byte-tools/take-uint (byte-tools/read-bytes-be f 0x2C 4))
        :loop-end (byte-tools/take-uint (byte-tools/read-bytes-be f 0x34 4))

      }
      ; default
      {
        :has-loop false
        :loop-start 0
        :loop-end 0
      })))

(defn get-loop-info
  "Returns loop information, as a map with the keys loop-flag, loop-start, and loop-end.
  If the file contains no loop data, or is in an unsupported ADX format, returns all values as 0.

  Values:
  * has-loop: boolean
  * loop-start: position of the beginning of the loop, in bytes
  * loop-end: position of the end of the loop, in bytes"
  [f]
  (case (get-version f)
    0x0300 (get-loop-info-3 f)
    0x0400 (get-loop-info-4 f)
    ; Note that encrypted files aren't actually supported yet
    ; Don't try if you value your ears!
    0x0408 (get-loop-info-4 f)
    {
      :has-loop false
      :loop-start 0
      :loop-end 0
    }))

(defn get-cutoff
  "Returns the cutoff frequency for File `f`, as a Long.
  MultimediaWiki says this is always 500Hz in practice."
  [f]
  (byte-tools/take-ushort (byte-tools/read-bytes-be f 0x10 2)))

(defn get-channel-count
  "Returns the number of the channels in File `f`, usually 1 (mono) or 2 (stereo)."
  [f]
  (byte-tools/take-ubyte (byte-tools/read-bytes-be f 7 1)))

(defn get-sample-rate
  "Returns the sample rate of File `f`."
  [f]
  (byte-tools/take-uint (byte-tools/read-bytes-be f 8 4)))

(defn get-sample-count
  "Returns the number of samples in File `f`."
  [f]
  (byte-tools/take-uint (byte-tools/read-bytes-be f 0xc 4)))

; TODO make this overload on input type, so a File can be passed in
(defn calculate-coefficients
  "Calculates the linear predictor coefficients given the cutoff frequency and the sample rate.
  Returns a vector of [coefficient_1 coefficient_2], both values as signed integers."
  [cutoff-filter sample-rate]
  (let [sqrt2 (Math/sqrt 2) z (Math/cos (/ (* (* 2 Math/PI) cutoff-filter) sample-rate))
        a (- sqrt2 z) b (- sqrt2 1) c (/ (- a (Math/sqrt (* (+ a b) (- a b)))) b)]
        [(int (Math/floor (* c 8192)))
         (int (Math/floor (* c (* c -4096))))]))

(defn- expand-sample
  "Calculates a full sample from a 4-bit nibble."
  [nibble scale coef1 coef2 hist1 hist2]
  (let [sample-delta (* nibble scale)
        predicted-sample (bit-shift-right (+ (* coef1 hist1) (* coef2 hist2)) 12)]
        (byte-tools/clamp16 (+ predicted-sample sample-delta))))

(defn read-samples-from-frame
  "Reads audio samples from a frame.
  Each frame (usually 18 bytes) consists of one unsigned short representing the scale of the nibbles in the frame, followed by 16 bytes representing 32 encoded samples.
  Returns a vector of signed Short samples along with the last two decoded samples."
  [frame hist1 hist2 coef1 coef2]
  ; note that the value of the scale is incremented by 1 over the value extracted from the frame
  (let [scale (+ (byte-tools/take-ushort frame) 1)]
    ; hist1 and hist2 are the two previous samples in the array, used to compute the predicted sample
    (loop [hist1 hist1 hist2 hist2 index 2 output []] ; index begins at 2 because we already read bytes 0 and 1 to form the scale
      (if (>= index (.capacity frame))
        ; Return the output as well as hist1 and hist2 values that can be used to calculate
        ; the next sample
        [output [hist1 hist2]]
        (let [byt (.get frame index)
              high-nibble (byte-tools/get-high-nibble byt)
              low-nibble (byte-tools/get-low-nibble byt)
              high-sample (expand-sample high-nibble scale coef1 coef2 hist1 hist2)
              low-sample (expand-sample low-nibble scale coef1 coef2 high-sample hist1)]
          (recur
            ; pass the two calculated samples as hist1 and hist2 to the next pair of samples
            low-sample
            high-sample
            (inc index)
            (into output [high-sample low-sample])))))))

(defn- process-frame
  "Reads a new frame from the provided InputStream `stream` into the provided Byte[] `frame-buffer` and passes it to read-samples-from-frame, returning that function's return value."
  [stream frame-buffer frame-size history-pair coefficients]
  (.read stream frame-buffer 0 frame-size)
  (apply (partial read-samples-from-frame (ByteBuffer/wrap frame-buffer)) (concat history-pair coefficients)))

(defn- read-interleaved-samples
  [stream frame-buffer frame-size history-samples coefficients]
  (let [
      stream-array (repeat (count history-samples) stream)
      frame-buffer-array (repeat (count history-samples) frame-buffer)
      frame-size-array (repeat (count history-samples) frame-size)
      coefficient-array (repeat (count history-samples) coefficients)
    ]
    (let [results (map #(apply process-frame %) (map vector stream-array frame-buffer-array frame-size-array history-samples coefficient-array))]
      ; return an array of all sample arrays, and an array of all history sample arrays
      [(map first results) (map last results)])))

(defn- write-interleaved-samples
  [stream output frame-buffer frame-size history-samples coefficients]
  (let [
      ; output-buffer is the size of the number of channels times the size of each decoded frame
      ; For example, for a mono file, output-buffer would be 32 2-byte samples (64 bytes),
      ; for a stereo file it would be 64 samples, etc. 
      output-buffer (ByteBuffer/allocate (* (count history-samples) (* 4 (- (alength frame-buffer) 2))))
      [decoded-samples new-history-samples] (read-interleaved-samples stream frame-buffer frame-size history-samples coefficients)
    ]
    (doseq [sample (apply interleave decoded-samples)]
      (.putShort output-buffer sample))
    (.write output (.array output-buffer) 0 (.capacity output-buffer))
    new-history-samples))

(defn decode-file
  "Decodes the provided File, writing output to the provided OutputStream.
   The output argument can be any subclass of OutputStream (e.g. BufferedOutputStream, ByteArrayOutputStream, etc).
   Output will be created as raw PCM audio, using 16-bit signed samples.
   The original channel count and frequency are maintained when decoding."
   ([f output]
    (decode-file f output 2))
   ([f output loops]
   (let [input (new RandomAccessFile f "r") ; readable input stream
         loop-data (get-loop-info f)
         offset (get-stream-offset f) ; the position in the file at which actual encoded ADX content begins
         channels (get-channel-count f) ; number of channels in the file, usually just 1 for mono or 2 for stereo
         frequency (get-sample-rate f)
         cutoff (get-cutoff f)
         samples (get-sample-count f)
         frame-size (get-frame-size f)
         samples-per-frame (* 2 (- frame-size 2))
         coefficients (calculate-coefficients cutoff frequency)
         frame (make-array Byte/TYPE frame-size)] ; Byte array to use to store each frame prior to decoding
         (.seek input offset)
         (loop [
            history-samples (repeat channels [0 0]) ; Sets of history samples for calculating predicted samples, one pair per channel
            loop-count 1
            bytes-played offset
          ]
          (if-not (>= bytes-played (.length input))
            (do
              (let [history-samples (write-interleaved-samples input output frame frame-size history-samples coefficients)
                    ; This is dependent on loops always occurring on frame boundaries
                    ; Please feel free to kick me later when this fragile assumption breaks!
                    loop-done? (= bytes-played (:loop-end loop-data))
                    new-loop-count (if loop-done? (inc loop-count) loop-count)
                    looping-done? (= loops loop-count)
                    new-bytes-played (if (and (:has-loop loop-data) loop-done? (not looping-done?))
                      (:loop-start loop-data) (+ bytes-played (* channels frame-size)))]
                (if (and (:has-loop loop-data) loop-done?)
                  (.seek input (:loop-start loop-data)))
                (recur
                  history-samples
                  new-loop-count
                  new-bytes-played))))))))
