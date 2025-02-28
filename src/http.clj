(ns http
  (:require
   [system]
   [cheshire.core]
   [clojure.string :as str]
   [hiccup.page]
   [org.httpkit.server :as server]
   [ring.middleware.head]
   [ring.middleware.params]
   [ring.middleware.cookies]
   [ring.util.codec :as codec]
   [ring.util.io]
   [ring.util.response]
   [http.routing]
   [http.formats]
   [org.httpkit.client :as http-client]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s])
  (:import [java.io BufferedWriter OutputStreamWriter ByteArrayInputStream ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.util.zip GZIPOutputStream]))

(set! *warn-on-reflection* true)

(defn handle-static [{meth :request-method uri :uri :as req}]
  (let [opts {:root "public"
              :index-files? true
              :allow-symlinks? true}
        path (subs (codec/url-decode (:uri req)) 8)]
    (-> (ring.util.response/resource-response path opts)
        (ring.middleware.head/head-response req))))

(defn parse-body [b]
  (when b
    (cond (string? b) (cheshire.core/parse-string b keyword)
          (instance? java.io.InputStream b) (cheshire.core/parse-stream (io/reader b :encoding "UTF-8") keyword)
          :else b)))

(defn resolve-operation [meth uri]
  (let [parts  (rest (str/split uri #"/"))
        cnt (count parts)]
    (cond
      (= 1 cnt) {:op (str (name meth) "-" (str/lower-case (first parts)))}
      (= 2 cnt) (let [id (second parts)]
                  (if (str/starts-with? id "$")
                    {:op (str (name meth) "-" (str/lower-case (first parts)) "-" (subs id 1))}
                    {:op (str (name meth) "-" (str/lower-case (first parts)) "-inst")
                     :params {:id id}}))
      (= 3 cnt) (let [id (second parts)]
                  {:op (str (name meth) "-" (str/lower-case (first parts)) "-" (nth parts 2))
                   :params {:id id}}))))

(comment
  (resolve-operation :get "/Package")
  (resolve-operation :get "/Package/id")
  (resolve-operation :get "/Package/$op")
  (resolve-operation :get "/Patient/pt-1/history")
  )

(defn parse-params [params]
  (let [params (when params (reduce (fn [acc [k v]] (assoc acc (keyword k) v)) {} (codec/form-decode params  "UTF-8")))]
    (if (map? params) params {})))

(defn encode-params [params]
  (codec/form-encode params))

(encode-params {:a 1 :b "$@#/"})

(parse-params "a=2%20;")

(defn response-body [ctx body]
  (cheshire.core/generate-string body))

(defn register-middleware [ctx mw-fn]
  (system/update-system-state ctx [:middlewares] (fn [mws] (conj (or mws #{}) mw-fn))))

(defn unregister-middleware [ctx mw-fn]
  (system/update-system-state ctx [:middlewares] (fn [mws] (disj mw-fn))))

(defn register-authorize-hook [context auth-fn]
  (system/register-hook context :http/authorize auth-fn auth-fn))


(defn clear-middlewares [ctx]
  (system/update-system-state ctx [:middlewares] (fn [mws] #{})))

(defn apply-middlewares [ctx req]
  (->> (system/get-system-state ctx [:middlewares])
       (reduce (fn [ctx mw]
                 (println :ctx-in ctx :mw mw)
                 (let [ctx (mw ctx req)]
                   (println :ctx-out ctx :mw mw)
                   ctx)
                 ) ctx)))

;; example work with context
(defn ctx-remote-addr [ctx]
  (system/ctx-get ctx ::remote-addr))

(defn parse-route [uri]
  (->> (str/split (str/replace uri #"(^/|/$)" "") #"/")
       (remove str/blank?)
       (mapv (fn [i]
               (if (str/starts-with? i ":")
                 [(keyword (subs i 1))]
                 i)))))

(s/def ::path string?)
(s/def ::method #(contains? #{:get :post :put :delete :patch :head} %))
(s/def ::fn ifn?)
(s/def ::endpoint (s/keys :req-un [::path ::method ::fn]))

;; make it macros with validation
(defn -register-endpoint [ctx {meth :method path :path f :fn :as opts}]
  (let [route (parse-route (str/replace path #"^/" ""))
        path (into route [meth])]
    (system/info ctx ::register-endpoint (str meth " " path " -> " f))
    (system/update-system-state
     ctx [:endpoints]
     (fn [x] (assoc-in x path opts)))))

;; TODO: use spec instrumentation to check params
(defmacro register-endpoint [context {_path :path _method :method _fn :fn :as endpoint}]
  `(let [result# (s/conform ~::endpoint ~endpoint)]
     (if (= :clojure.spec.alpha/invalid result#)
       (throw (ex-info "Invalid endpoint"
                       (s/explain-data ~::endpoint ~endpoint)))
       (-register-endpoint ~context ~endpoint))))


(defn -register-context-middleware [context {meth :method path :path f :fn :as opts}]
  (let [route (parse-route (str/replace path #"^/" ""))
        path (into route [meth])]
    (system/info context ::register-endpoint (str meth " " path " -> " f))
    (system/update-system-state
     context [:middlewares]
     (fn [x] (assoc-in x path opts)))))

;; TODO find all mw and call it in request - updating context
(defmacro register-context-middleware [context {_path :path _method :method _fn :fn :as mw}]
  `(-register-context-middleware ~context ~mw))


(defn clear-endpoints [ctx]
  (system/clear-system-state ctx [:endpoints]))

(defn unregister-endpoint [ctx {meth :method url :path}]
  (let [route (parse-route url)]
    (system/clear-system-state ctx (into [:endpoints] (conj route meth)))))

(defn get-routes [ctx]
  (system/get-system-state ctx [:endpoints]))

(defn resolve-endpoint [ctx meth url]
  (let [routes (get-routes ctx)]
    (http.routing/match [meth url] routes)))

(defn on-request-hooks [ctx params]
  (doseq [on-request-hook (system/get-hooks ctx ::on-request)]
    (when-let [f (:fn on-request-hook)]
      (f ctx params))))

;;TODO look into content header of request
(defn format-response [ctx resp]
  (if-not (and (:body resp) (or (map? (:body resp)) (vector? (:body resp))))
    resp
    (-> resp
     (update :body (fn [x] (if (or (vector? x) (map? x)) (cheshire.core/generate-string x) x)))
     (assoc-in [:headers "content-type"] "application/json"))))

(defn authorization-enabled? [context]
  (system/get-config context :enable-authorization))

(defn authenticated! [context]
  (assoc context ::authenticated true))

(defn authenticated? [context]
  (get context ::authenticated))

(defn authenticate [context op request]
  )

(defn authorize [context op request]
  ;; (println :authorize  :hooks (system/get-hooks context ::authorize))
  (or (:public op)
      (->> (system/get-hooks context ::authorize)
           (some
            (fn [[hook-id hook]]
              (let [res (hook context op request)]
                (system/info context ::auth-hook (str hook-id) res)
                res))))))

;; hook should return nil if does not want to handle this
(defn handle-unauthorized [context op request]
  (or
   (->> (system/get-hooks context ::unauthorized)
        (some
         (fn [[hook-id hook]]
           (let [res (hook context op request)]
             (system/info context ::auth-hook (str hook-id) res)
             res))))
   {:status 403}))

(defn handle-anonimous [context op request]
  ;; TODO
  )

(defn dispatch [system {meth :request-method uri :uri :as req}]
  (let [ctx (system/new-context system {::uri uri ::method meth ::remote-addr (:remote-addr req)})
        ctx (apply-middlewares ctx req)]
    (cond
      (and (contains? #{:get :head} meth) (str/starts-with? (or uri "") "/static/"))
      (handle-static req)

      :else
      (let [query-params (parse-params (:query-string req))]
        (if-let [{{f :fn :as op} :match params :params} (resolve-endpoint ctx meth uri)]
          (let [auth-ctx (authenticate ctx op req)]
            ;;
            (if-let [anonimous-response (when (and (not (:public op)) (not (authenticated? auth-ctx))) (handle-anonimous auth-ctx op req))]
              anonimous-response
              (if (and (authorization-enabled? auth-ctx) (not (authorize auth-ctx op req)))
                (handle-unauthorized system op req)
                (let [start (System/nanoTime)]
                  ;; TODO set context for logger
                  (system/info auth-ctx meth uri {:route-params params})
                  (on-request-hooks auth-ctx {:uri uri :method meth :query-params query-params})
                  (let [res (->>
                             (f auth-ctx (assoc (merge req op) :query-params query-params :route-params params))
                             (format-response auth-ctx))]
                    (system/info auth-ctx meth uri {:duration (/ (- (System/nanoTime) start) 1000000.0) :status (:status res)})
                    res)))))
          (do
            (system/info ctx meth (str uri " not found" {:http.status 404}))
            {:status 404
             :body (str (name meth) " " uri " is not found")}))))))

(defn stream [req cb]
  (server/with-channel req chan
    (server/on-close chan (fn [_status] ))
    (future
      (try
        (server/send! chan {:headers {"content-type" "application/x-ndjson" "content-encoding" "gzip"}} false)
        (let [array (ByteArrayOutputStream.)
              gzip (GZIPOutputStream. array true)
              wrtr (BufferedWriter. (OutputStreamWriter. gzip StandardCharsets/UTF_8))
              wr (fn [^String res]
                   (.write wrtr res)
                   (.write wrtr "\n")
                   (.flush wrtr)
                   (.flush array)
                   (server/send! chan (.toByteArray array) false)
                   (.reset array))]
          (cb wr)
          (.finish gzip)
          (.flush array)
          (server/send! chan (.toByteArray array) false))
        (catch Exception e (println :error e))
        (finally
          (server/close chan))))))


(defn json-content-type? [resp]
  (= "application/json" (get-in resp [:headers :content-type])))

(defn ndjson-content-type? [resp]
  (= "application/x-ndjson" (get-in resp [:headers :content-type])))

(defn request [ctx {path :path qp :query-params :as opts}]
  (let [url (str "http://localhost:" (system/get-system-state ctx [:port]) path)
        resp @(http-client/get url opts)]
    (system/info ctx ::get url)
    (update resp :body (fn [x]
                         (when-let [body (if (string? x) x (if (nil? x) nil (slurp x)))]
                           (cond (json-content-type? resp)
                                 (cheshire.core/parse-string body keyword)
                                 (ndjson-content-type? resp)
                                 (->> (str/split body #"\n")
                                      (mapv (fn [x] (cheshire.core/parse-string x keyword))))
                                 :else body)
                           )))))

(system/defmanifest
  {:description "http server module"
   :deps []
   :define-hook {::authorize {:args [::operation-definition ::request] :result ::authorized}}
   :config {:binding {:type "string" :default "127.0.0.1" :validator (complement empty?)}
            :port {:type "integer" :default 8080 :required true :validator pos-int?}
            :enable-authorization {:type "boolean"}}})

(system/defstart [context config]
  (let [binding (:binding config)
        port (:port config)]
    (system/info context ::start (str "start http server" config))
    ;; TODO: move to manifest
    (system/manifest-hook context ::on-request {:desc "This hook is called on request and passed method, uri and params"})
    {:server (server/run-server (fn [req] (#'dispatch context req)) {:ip binding :port port}) :binding binding :port port}))

(system/defstop [context state]
  (when-let [stop (:server state)]
    (stop)))


(comment

  (defn get-test
    [ctx req]
    {:status 200
     :body (response-body ctx {:message (java.util.Date.) :route-params (:route-params req)})})

  (defn logging-mw [ctx req]
    (println :HTTP (:uri req))
    ctx)

  (def context (system/start-system {:services ["http" "http.openapi"] :http {:port 8889}}))

  (system/get-system-state context [:port])
  (system/get-system-state context [:binding])

  context

  (system/stop-system context)

  (request context {:path "/api"})

  (register-middleware context #'logging-mw)

  (register-endpoint context {:method :get :path  "/test" :fn  #'get-test})

  (register-endpoint context {:method  :get :path "/Patient/:id" :fn #'get-test})

  (request context {:path "/test"})

  (get-routes context)

  (unregister-endpoint context {:method :get :path "/test"})

  (clear-endpoints context)



  (resolve-endpoint context :get "/test")
  (resolve-endpoint context :get "/Patient/pt-1")

  (system/get-system-state context [:endpoints])

  (clear-middlewares context)

  (time (request context {:path "/test"}))
  (request context {:path "/Patient/pt-1"})


  (parse-route "/Patient/:id")

  (defn on-request-hook [ctx params] (println :HOOK params))

  (system/register-hook context ::on-request #'on-request-hook)

  (request context {:path "/api"})
  )
