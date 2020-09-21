(defproject slackbot-vici "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-http "3.9.1"]
                 [http-kit "2.4.0-alpha4"]
                 [compojure "1.6.1"]

                 ; Databases
                 [org.clojure/java.jdbc "0.7.9"]
                 [org.postgresql/postgresql "42.2.5"]

                 [ring "1.7.1"]
                 [ring/ring-json "0.4.0"]
                 [org.julienxx/clj-slack "0.6.3"]]
  :main ^:skip-aot slackbot-vici.core
  :min-lein-version "2.0.0"
  :uberjar-name "slackbot-vici.jar"
  :profiles {:dev {:main slackbot-vici.core/-dev-main}})
