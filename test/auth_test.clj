(ns auth-test
  (:require [system]
            [http]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string]
            [matcho.core :as matcho]))

(system/ensure-context {:services ["http"] :http {:port 8282 :enable-authorization true :enable-authentication true}})

(declare context)
(declare ensure-context)
(declare reload-context)

(defn start-stop-system-fixture [f]
  (ensure-context)
  (f)
  (system/stop-system context))

(defn reload-system-and-clean-resources-fixture [f]
  (reload-context)
  (f))

(use-fixtures :once start-stop-system-fixture)
(use-fixtures :each reload-system-and-clean-resources-fixture)

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

(defn authorize
  [context req]
  (not (clojure.string/starts-with? (:path req) "/admin")))

(defn get-index [context req]
  {:status 200 :body "Here"})

(deftest test-auth-flow
  (reload-context)

  (http/register-authenticate-hook context #'authenticate-my-user)
  (http/register-handle-anonymous-hook context #'handle-anonymous)
  (http/register-authorize-hook context #'authorize)

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


(defn get-patient [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(defn get-patients [context req]
  (http/format-response context {:status 200 :body (:route-params req)}))

(deftest authorize-hook-test

  (reload-context)

  (http/register-authorize-hook context #'authorize)

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

  (is (http/authorize-hooks context))

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
