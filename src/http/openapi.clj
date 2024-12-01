(ns http.openapi
  (:require [system]
            [http :as http]))

(defn get-open-api [context req]
  {:status 200
   :body (http/get-routes context)})

(system/defstart [context config]
  (http/register-endpoint context {:method :get :path  "/api" :fn  #'get-open-api})
  {})

(system/defmanifest
  {:description "add openapi to the http"})
