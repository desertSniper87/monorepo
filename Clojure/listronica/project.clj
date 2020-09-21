(defproject webdev "0.1.0-SNAPSHOT"
  :description "TODO List app created for LispCast Web Development in Clojure."
  :url "https://purelyfunctional.tv/web-dev-in-clojure"
  :license {:name "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ring "1.7.1"]
                 [compojure "1.5.1"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4.1208.jre7"]
                 [hiccup "1.0.5"]]

  :min-lein-version "2.0.0"

  :uberjar-name "webdev.jar"

  :main webdev.core

  :profiles {:dev
             {:source-paths ["src" "dev"]
              :main webdev.dev}})
