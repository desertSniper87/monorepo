(ns slackbot-vici.core
  (:require [slackbot_vici.message.model :as messages])
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.reload :refer [wrap-reload]]
            [compojure.core :refer [GET defroutes POST ANY]]
            [ring.handler.dump :refer [handle-dump]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.json :refer [wrap-json-params wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response]]
            [clj-slack.chat :as chat]))

(defn url-verification-event
  [json-body]
  (response {:challenge (:challenge json-body)}))

(defn bot [request]
  (println "json-body: " (:body request))
  (let [json-body (:body request)
        type (:type json-body)]
    (cond
      (= "url_verification" type)
      (url-verification-event json-body)
      (= "event_callback" type)
      ; immediate http 2xx response needed
      ;(response "we are processing!!!"))))
      ; concurency needed; will implement later
       (let [conn {:api-url "https://slack.com/api" :token "xoxb-609755695890-623945872567-DSwwwMzqpLiOXFWupJKxgFlU"}
             inner-type (get-in json-body [:event :type])
             channel (get-in json-body [:event :channel])]
         (println inner-type channel)
         ;(response "we are processing...")))))
         ;(println (clj-slack.chat/post-message conn channel "your task is processing"))))))
         (chat/post-message conn (str channel) "your task is processing...")))))


(defroutes routes
  ; for testing request data
  (ANY "/request" [] (wrap-json-body handle-dump {:keywords? true}))
  (POST "/bot" [] (wrap-json-response (wrap-json-body bot {:keywords? true}))))

(def app (wrap-params routes))

(defn -main
  [port]
  (clojure.java.jdbc/execute! messages/db [messages/create-table-sql])
  (let [port (Integer/parseInt port)]
    (jetty/run-jetty app {:port port})))

(defn -dev-main
  [port]
  (let [port (Integer/parseInt port)]
    (jetty/run-jetty (wrap-reload #'app) {:port port})))