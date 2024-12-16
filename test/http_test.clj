(ns http-test
  (:require [system]
            [http]
            [clojure.test :refer [deftest is testing]]
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

(deftest test-register-endpoint
  (ensure-context)

  (testing "when path is a string"
    (is (macroexpand '(http/register-endpoint
                       context
                       {:method :get
                        :path "/foobar"
                        :fn identity}))
        "Unexpected error during macro expansion"))
  (testing "when path is a list"
    (is (macroexpand '(http/register-endpoint
                        context
                        {:method :get
                         :path (str "/foo/" 123)
                         :fn identity}))
        "Unexpected error during macro expansion"))
  (testing "when endpoint doesn't conform to the schema"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid endpoint"
         (http/register-endpoint
          context
          {:method :get :path nil :fn identity}))
        "Non-conforming path must throw")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid endpoint"
         (http/register-endpoint
          context
          {:method "get" :path "/" :fn identity}))
        "Non-conforming method must throw")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid endpoint"
         (http/register-endpoint
          context
          {:method :get :path "/" :fn nil}))
        "Non-conforming fn must throw")))

(deftest test-http
  (ensure-context)

  (http/unregister-endpoint context {:method :get :path "/"})

  (matcho/match
   (http/request context {:path "/"})
   {:status 404})

  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patients :params {:_id {:type "string"}}})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patient})
  (http/register-endpoint context {:method :get :path "/admin/Patient/:id" :fn #'get-patient})

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
