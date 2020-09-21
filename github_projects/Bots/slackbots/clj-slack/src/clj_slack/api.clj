(ns clj-slack.api
  (:require [clj-slack.core :refer [slack-request stringify-keys]])
  (:refer-clojure :exclude [test]))

(defn test
  "Helps you test your calling code.
  Optional arguments are:
  - error: error response to return
  - foo: example property to return"
  ([connection]
   (test connection {}))
  ([connection optionals]
   (->> optionals
        stringify-keys
        (slack-request connection "api.test"))))
