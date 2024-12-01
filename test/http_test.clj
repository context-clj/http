(ns http-test
  (:require [system]
            [http]
            [clojure.test :as t]
            [matcho.core :as matcho]))

(defn get-index [context req]
  {:status 200 :body "Here"})

(defn get-patient [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn get-patients [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(t/deftest test-http
  (def context (system/start-system {:services ["http"] :http {:port 8181}}))

  context

  (matcho/match
   (http/request context {:path "/"})
   {:status 404})

  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patients :params {:_id {:type "string"}}})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patient})

  (http/get-routes context)

  (matcho/match
   (http/request context {:path "/"})
   {:status 200
    :body "Here"})

  (matcho/match
   (http/request context {:path "/Patient/pt-1"})
   {:body {:id "pt-1"}})

  (system/stop-system context)



  )
