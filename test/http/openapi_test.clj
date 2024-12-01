(ns http.openapi-test
  (:require [system]
            [http]
            [clojure.test :as t]
            [matcho.core :as matcho]))

(t/deftest test-openapi

  (def context (system/start-system {:services ["http" "http.openapi"] :http {:port 5558}}))

  (matcho/match
   (http/request context {:path "/api"})
   {:status 200})

  (system/stop-system context)

  )
