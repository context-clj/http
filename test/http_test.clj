(ns http-test
  (:require [system]
            [http]
            [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [matcho.core :as matcho]))

(system/ensure-context {:services ["http"] :http {:port 8181 :enable-authorization true :enable-authentication true}})

(comment
  (ensure-context)
  (reload-context)
  context
  (http/authorization-enabled? context))

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

(defn authorize
  [context req]
  (not (clojure.string/starts-with? (:path req) "/admin")))

(defn get-index [context req]
  {:status 200 :body "Here"})

(defn get-patient [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn get-patients [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn rt-middleware [context {rp :route-params}]
  (system/ctx-set context [:resource_type] rp))

(deftest test-middleware
  (reload-context)

  (system/register-hook context :http/authorize ::authorize #'authorize)

  (matcho/match
   (http/request context {:path "/"})
    {:status 404})

  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})

  (matcho/match
   (http/request context {:path "/"})
    {:status 200})

  (http/unregister-endpoint context {:method :get :path "/"})

  (matcho/match
   (http/request context {:path "/"})
    {:status 404})

  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})

  (is (system/get-hooks context :http/authorize))

  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patients :params {:_id {:type "string"}}})
  (http/register-endpoint context {:method :get :path "/Patient/:id" :fn #'get-patient})
  (http/register-endpoint context {:method :get :path "/admin/Patient/:id" :fn #'get-patient})
  (http/register-endpoint context {:method :get :path "/admin/public" :fn #'get-patient  :public true})

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

  (matcho/match
   (http/request context {:path "/admin/public"})
    {:status 200}))

(defn authenticate-my-user
  [context req]
  (let [user (-> req :query-params :user)]
    (if (= "my-user" user)
      (http/authenticated! (assoc context ::user {:id user}))
      context)))

(defn handle-anonymous [context req]
  {:status 302
   :headers {"Location" "auth-redirect"}})

(defn auth-redirect [context req]
  {:status 200
   :body "You have been redirected!"})

(deftest test-auth-flow
  (reload-context)

  (system/register-hook context :http/authenticate ::authenticate #'authenticate-my-user)
  (system/register-hook context :http/handle-anonymous ::handle-anonymous #'handle-anonymous)
  (system/register-hook context :http/authorize ::authorize #'authorize)

  (http/register-endpoint context {:method :get :path "/private" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/admin" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/public" :fn #'get-index :public true})
  (http/register-endpoint context {:method :get :path "/auth-redirect" :fn #'auth-redirect :public true})

  (testing "unauthenticated requests to private endpoints"
    (matcho/match
     (http/request context {:path "/private?user=other-user"})
      {:status 200
       :body "You have been redirected!"})

    (matcho/match
     (http/request context {:path "/admin"})
      {:status 200
       :body "You have been redirected!"}))

  (testing "authenticated requests to private endpoints"
    (testing "authorized"
      (matcho/match
       (http/request context {:path "/private?user=my-user"})
        {:status 200}))

    (testing "not authorized"
      (matcho/match
       (http/request context {:path "/admin?user=my-user"})
        {:status 403})))

  (testing "unauthenticated requests to public endpoints"
    (matcho/match
     (http/request context {:path "/public"})
      {:status 200})))
