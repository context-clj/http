(ns http-test
  (:require [system]
            [http]
            [clojure.test :as t]
            [clojure.string]
            [matcho.core :as matcho]))

(system/ensure-context {:services ["http"] :http {:port 8181 :enable-authorization true}})

(comment
  (reload-context)
  context

  (ensure-context)

  (http/authorization-enabled? context)

  )

(defn get-index [context req]
  {:status 200 :body "Here"})

(defn get-patient [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn get-patients [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn authorize 
  [context op req]
  (not (clojure.string/starts-with? (:path op) "/admin")))

(defn rt-middleware [context {rp :route-params}]
  (system/ctx-set context [:resource_type] rp))

(t/deftest test-http
  (ensure-context)

  (http/unregister-endpoint context {:method :get :path "/"})

  (matcho/match
   (http/request context {:path "/"})
   {:status 404})

  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patients :params {:_id {:type "string"}}})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patient})
  (http/register-endpoint context {:method :get :path "/admin/Patient/:id" :fn #'get-patient})


  (http/register-middleware context {:method :get :path "admin/:resource_type/:id" :fn #'rt-middleware})

  (system/register-hook context :http/authorized ::auth #'authorize)

  (http/get-routes context)

  (matcho/match
   (http/request context {:path "/"})
   {:status 200
    :body "Here"})

  (matcho/match
   (http/request context {:path "/Patient/pt-1"})
   {:body {:id "pt-1"}})

  (matcho/match
   (http/request context {:path "/admin/Patient/pt-1"})
   {:status 403})


  )
