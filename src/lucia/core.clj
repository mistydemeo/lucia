(ns lucia.core
  (:gen-class)
  (require [clojure.java.io :as io])
  (require [lucia.adx :as adx])
  (import (javax.sound.sampled AudioFormat AudioFormat$Encoding AudioSystem SourceDataLine)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [song-file (io/file (or (first args) (io/resource "flute-alto-lick.adx")))
        sample-rate (adx/get-sample-rate song-file)
        channel-count (adx/get-channel-count song-file)
        audio-format (new AudioFormat AudioFormat$Encoding/PCM_SIGNED sample-rate 16 channel-count (* 2 channel-count) sample-rate false)
        output (AudioSystem/getSourceDataLine audio-format)
        line (reify SourceDataLine
               (write [self b off len]
                 (.write #^SourceDataLine output b off len)))]
        (.open output audio-format)
        (.start output)
        (adx/decode-file song-file line)))
