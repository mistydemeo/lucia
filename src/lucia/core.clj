(ns lucia.core
  (:gen-class)
  (require [clojure.java.io :as io]
           [lucia.play :as play]))

(def commands
  #{"play"})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [command (first args) songs (rest args)]
    (if-not command
      (do 
        (.println *err* "No command specified!")
        (System/exit 1)))
    (if-not (contains? commands command)
      (do
        (binding [*out* *err*] (println "Unrecognized command:" command))
        (System/exit 1)))

    (case command
          "play" (play/process-songs-cli songs))))
