(ns http-test
  (:require [system]
            [http]
            [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [matcho.core :as matcho]))

(system/ensure-context {:services ["http"] :http {:port 8181}})

(declare context)
(declare ensure-context)
(declare reload-context)

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
        "Non-conforming fn must throw")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid endpoint"
         (http/register-endpoint
          context
          {:method :get :path "/" :fn identity :middleware #{identity}}))
        "Non-conforming middleware must throw")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid endpoint"
         (http/register-endpoint
          context
          {:method :get :path "/" :fn identity :middleware "mw1"}))
        "Non-conforming middleware must throw")))

(deftest middleware-test
  (testing ""
    (let [handler (fn [_ctx req] {:status 200
                                  :body (:body req)})]
      (http/register-endpoint
       context
       {:method :post :path "/" :fn handler :middleware [http/parse-body]})

      (matcho/match
        (http/dispatch context {:request-method :post
                                :uri "/"
                                :body "{\"hello\": \"world\"}"})
        {:status 200
         :body {:hello "world"}}))))

(defn get-index [context req]
  {:status 200 :body "Here"})

(defn on-endpoint [context endpoint opts]
  (when (:menu endpoint)
    (system/set-system-state context [:menu (:menu endpoint)] endpoint)))

(defn get-menu [context]
  (vals (system/get-system-state context [:menu])))

(deftest test-endpoint-subs
  (reload-context)

  (http/subscribe-to-endpoint-register context {:fn #'on-endpoint})

  (http/register-endpoint context {:method :get :path "/a" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/b" :fn #'get-index :menu "Item 1"})
  (http/register-endpoint context {:method :get :path "/c" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/d" :fn #'get-index :menu "Item 2"})

  (matcho/match
    (get-menu context)
    [{:menu "Item 1"}
     {:menu "Item 2"}]))

(defn on-request [context request opts]
  (swap! (:state opts) conj (:uri request)))

(deftest test-request-subs
  (reload-context)

  (def state (atom []))
  (http/register-endpoint context {:method :get :path "/" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/a" :fn #'get-index})
  (http/register-endpoint context {:method :get :path "/b" :fn #'get-index})
  (http/subscribe-to-request context {:fn #'on-request :state state})

  (matcho/match (http/request context {:path "/"}) {:status 200 :body "Here"})
  (matcho/match (http/request context {:path "/a"}) {:status 200 :body "Here"})
  (matcho/match (http/request context {:path "/b"}) {:status 200 :body "Here"})

  (is (= ["/" "/a" "/b"] @state)))


