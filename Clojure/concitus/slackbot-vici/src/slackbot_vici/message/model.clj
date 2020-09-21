(ns slackbot_vici.message.model
  (:require [clojure.java.jdbc :as dbl]))

(def db {:dbtype   "postgresql"
         :dbname   "vicidialdb"
         :host     "localhost"
         :user     "vicidial"
         :password "vicidial"})

(def create-table-sql (dbl/create-table-ddl :messages [[:message_id :serial "PRIMARY KEY"]
                                                       [:sender "VARCHAR(100)"]
                                                       [:message_text "TEXT"]
                                                       [:date_created "TIMESTAMPTZ NOT NULL DEFAULT now()"]]))
