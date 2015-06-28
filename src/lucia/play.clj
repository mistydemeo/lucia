(ns lucia.play
  "Provides functions to play back audio."
  (:require [clojure.java.io :as io]
            [lucia.adx :as adx]
            [lucia.sega-cd :as sega-cd]
            [lucia.helpers :refer [identify]])
  (:import (javax.sound.sampled AudioFormat AudioFormat$Encoding AudioSystem SourceDataLine)))

(defn print-song-data
  [songdata]
  (if-not (empty? songdata)
    (do
      (println "Name:" (:title songdata))
      (println "File type:" (:type songdata))
      (println "Filename:" (:filename songdata)))))

(defn play-song
  [song]
  (let [sample-rate (adx/get-sample-rate song)
        channel-count (adx/get-channel-count song)
        audio-format (new AudioFormat AudioFormat$Encoding/PCM_SIGNED sample-rate 16 channel-count (* 2 channel-count) sample-rate false)
        output (AudioSystem/getSourceDataLine audio-format)
        line (reify SourceDataLine
               (write [self b off len]
                 (.write #^SourceDataLine output b off len)))]
        (.open output audio-format)
        (.start output)
        (adx/decode-file song line)))

(defn process-songs-cli
  [songs]
  (doseq [song songs]
    (let [song-file (io/file song)]
      (if (adx/file-valid? song-file)
        (let [songdata (identify song-file)]
          (print-song-data songdata)
          (play-song song-file))
        (binding [*out* *err*] (println "Unrecognized file:" song))))))
