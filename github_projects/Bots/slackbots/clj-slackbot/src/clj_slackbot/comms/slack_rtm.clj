(ns clj-slackbot.comms.slack-rtm
  (:require [clojure.core.async :as async :refer [go go-loop]]
            [clj-http.client :as http]
            [gniazdo.core :as ws]
            [cheshire.core :refer [parse-string generate-string]]
            [clj-slackbot.util :as util]))

(def ^:private rtm-socket-url
  "https://slack.com/api/rtm.start")

(defn get-websocket-url [api-token]
  (let [response (-> (http/get rtm-socket-url
                               {:query-params {:token      api-token
                                               :no_unreads true}
                                :as :json})
                     :body)]
    (when (:ok response)
      (:url response))))

(defn connect-socket [url]
  (let [in (async/chan)
        out (async/chan)
        socket (ws/connect
                 url
                 :on-receive
                 (fn [m]
                   (async/put! in (parse-string m true)))
                 :on-error
                 (fn [_]
                   (async/close! in)))]
    (go-loop []
      (let [m (async/<! out)
            s (generate-string m)]
        (ws/send-msg socket s)
        (recur)))
    [in out]))

(defn- can-handle? [data prefix]
  (when-let [text (:text data)]
    (.startsWith text prefix)))


(defn start [{:keys [api-token prefix]}]
  (let [cin (async/chan 10)
        cout (async/chan 10)
        url (get-websocket-url api-token)
        counter (atom 0)
        next-id (fn []
                  (swap! counter inc))
        shutdown (fn []
                   (async/close! cin)
                   (async/close! cout))
        mk-timeout #(async/timeout 15000)]
    (when (clojure.string/blank? url)
      (throw (ex-info "Could not get RTM Websocket URL" {})))

    (println ":: got websocket url:" url)

    ;; start a loop to process messages
    (go-loop [[in out] (connect-socket url)
              ping-count 0
              to (mk-timeout)]
      ;; get whatever needs to be done for either data coming from the socket
      ;; or from the user
      (let [[v p] (async/alts! [cout in to])]
        ;; if something goes wrong, just die for now
        ;; we should do something smarter, may be try and reconnect
        (if (= p to)
          ;; time to ping
          (do
            (println ":: ping? pending pings:" ping-count)
            (async/>! out {:id   (next-id)
                           :type "ping"
                           :ts   (System/currentTimeMillis)})
            (if (> ping-count 5)
              (recur (connect-socket url) 0 (mk-timeout))
              (recur [in out] (inc ping-count) (mk-timeout))))
          (if (nil? v)
            (do
              (println "A channel returned nil, may be its dead? Leaving loop.")
              (shutdown))
            (do
              (if (= p cout)
                ;; the user sent us something, time to send it to the remote end point
                (async/>! out {:id      (next-id) :type "message"
                               :channel (get-in v [:meta :channel])
                               :text    (-> v
                                            util/format-result-for-slack)})

                ;; the websocket has sent us something, figure out if its of interest
                ;; to us, and if it is, send it to the evaluator
                (do
                  (println ":: incoming:" v)
                  (if (= (:type v) "pong")
                    (println ":: pong! latency: " (- (System/currentTimeMillis) (:ts v)) "ms.")
                    (when (can-handle? v prefix)
                      (async/>! cin {:input (subs (:text v) 1)
                                     :meta  v})))))
              (recur [in out]
                     (if (= (:type v) "pong")
                       (dec ping-count) ping-count)
                     (mk-timeout)))))))
    [cin cout shutdown]))
