(ns lucia.core
  (:gen-class)
  (require [clojure.java.io :as io])
  (require [clj-yaml.core :as yaml])
  (require [pandect.algo.md5 :refer [md5]]))

(def songdata
  (yaml/parse-string (io/input-stream (io/resource "songdata.yaml"))))

(defn identify
  [song]
  "Attempts to identify a passed song File by looking it up in the songdata collection.
  Returns either a map with song metadata, or nil if the song is unrecognized.
  Song metadata maps contain the keys:
    * :title
    * :type (\"adx\" or \"pcm\")
    * :filename"
  (let [buffer (byte-array 8192)]
    (.read (io/input-stream song) buffer 0 8192)
    ((keyword (md5 buffer)) songdata)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
