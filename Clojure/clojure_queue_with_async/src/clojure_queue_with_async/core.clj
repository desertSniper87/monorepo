(ns clojure-queue-with-async.core
  (:require [clojure.core.async :as async]))

(defn spawn-data-consumer-goblock
  [data-chan]
  ;; This creates a goblock, which oportunistically uses
  ;; a fixed thread pool of size 8. Don't do blocking I/O operations
  ;; inside a goblock and use non blocking version of channel get/put
  ;; operations, i.e.: <!, >!, put! etc.
  (async/go
    (println "Spawned consumer goblock ...")
    (loop [data (async/<! data-chan)]
      (if data
        (do (println "Received data: " data)
            ;; Sleep for some time.
            (async/<! (async/timeout 250))
            (recur (async/<! data-chan)))))
    (println "Closing consumer goblock ...")))

(defn spawn-data-producer-thread
  [data-chan finish-marker-chan]
  ;; This spawns a dedicated JVM thread, out the fixed threadpool
  ;; dedicated to async operations. It's ok to block indefinitely
  ;; inside such a thread. You must use blocking version of get/put
  ;; operations (i.e. <!!, >!!, put!! etc.) here, as we are outside
  ;; of go-blocks.
  (async/thread
    (println "Spawned producer thread ...")
    (doseq [i (range 1 16)]
      (async/>!! data-chan (str "plumbus-no-" i))
      ;; Sleep for some time.
      (async/<!! (async/timeout 1000)))
    (println "Closing producer thread ...")
    (async/>!! finish-marker-chan :done)))


(defn -main [& args]
  c(let [data-chan          (async/chan)
         finish-marker-chan (async/promise-chan)]
     ;; Spawn producers and consumers.
     (spawn-data-consumer-goblock data-chan)
     (spawn-data-producer-thread data-chan finish-marker-chan)

     ;; Data can be produced and put into channels from any thread,
     ;; can be consumed only once by default (although there are
     ;; facilities in core.async to do otherwise).
     (async/>!! data-chan "plumbus-from-main-thread")

     ;; Wait for some data to be put in finish-marker-chan.
     (async/<!! finish-marker-chan)

     ;; Close all channels.
     (async/close! data-chan)
     (async/close! finish-marker-chan)))
