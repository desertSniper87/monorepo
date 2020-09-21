(ns webdev.dev
  (:require [webdev.item.model :as items])
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.reload :refer [wrap-reload]]
            [webdev.core :as core]))

(defn -main [port]
  (items/create-table core/db)
  (jetty/run-jetty (wrap-reload #'core/app) {:port (Integer. port)}))
