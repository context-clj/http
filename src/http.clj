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
   [ring.middleware.content-type]
   [ring.util.codec :as codec]
   [ring.util.io]
   [ring.util.response]
   [http.routing]
   [http.formats]
   [org.httpkit.client :as http-client]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s])
  (:import [java.io BufferedWriter OutputStreamWriter ByteArrayInputStream ByteArrayOutputStream]
           [java.net URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.util.zip GZIPOutputStream]))

(set! *warn-on-reflection* true)

(defn handle-static [{meth :request-method uri :uri :as req}]
  (let [opts {:root "public"
              :index-files? true
              :allow-symlinks? true}
        path (subs (codec/url-decode (:uri req)) 8)]
    (-> (ring.util.response/resource-response path opts)
        (ring.middleware.head/head-response req)
        (ring.middleware.content-type/content-type-response req)
        (ring.util.response/charset "utf-8"))))

(defn set-cookie [response name value & [params]]
  (ring.util.response/set-cookie response name value params))

(defn get-cookie [request name]
  (get-in request [:cookies name :value]))

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

(def form-decode codec/form-decode)

(defn parse-params [params]
  (let [params (when params (reduce (fn [acc [k v]] (assoc acc (keyword k) v)) {} (codec/form-decode params  "UTF-8")))]
    (if (map? params) params {})))

(defn encode-params [params]
  (codec/form-encode params))


(defn url-decode [^String encoded-string]
  (URLDecoder/decode encoded-string "UTF-8"))

(defn request-form [{b :body :as request}]
  (when b
    (let [body (if (string? b) b (slurp b))]
      (parse-params body))))

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


(defn subscribe-to-endpoint-register
  "subscribe to endpoint register (fn [context endpoint h])"
  [context {f :fn :as h}]
  (system/set-system-state context [:on-endpoint-register f] h))

(defn unsubscribe-from-endpoint-register   [context {f :fn}]
  (system/clear-system-state context [:on-endpoint-register f]))

(defn register-endpoint-subscriptions [context]
  (system/get-system-state context [:on-endpoint-register]))

(defn notify-endpoint-register-subscriptions [context endpoint & [{focus-subs :focus-subs :as opts}]]
  (doseq [[f f-opts] (register-endpoint-subscriptions context)]
    (when (or (nil? focus-subs)
              (contains? focus-subs f))
      (f context endpoint f-opts))))

;;TODO notify unregister subscriptions


;; make it macros with validation
(defn -register-endpoint [context {meth :method path :path f :fn :as opts}]
  (let [route (parse-route (str/replace path #"^/" ""))
        path (into route [meth])]
    (system/info context ::register-endpoint (str meth " " path " -> " f))
    (system/update-system-state context [:endpoints] (fn [x] (assoc-in x path opts)))
    (notify-endpoint-register-subscriptions context opts)))

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

(defn collect-endpoints-from-ns [ns]
  (->> (ns-interns ns)
       (filter #(:http (meta (second %))))
       (remove nil?)
       (mapv (fn [[_nm v]]
               (let [m (:http (meta v))]
                 (merge {:fn v :method (or (:method m) :get)} m))))))

(defn register-ns-endpoints [context ns]
  (doseq [endpoint (collect-endpoints-from-ns ns)]
    (http/register-endpoint context endpoint)))

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

(defn authentication-enabled? [context]
  (system/get-config context :enable-authentication))

(defn authenticated! [context]
  (assoc context ::authenticated true))

(defn authenticated? [context]
  (get context ::authenticated))

(defn register-authenticate-hook [context hook-var-fn]
  (system/register-hook context ::authenticate hook-var-fn hook-var-fn))

;; authenticate hooks should return context
(defn authenticate [context request]
  (if-let [hooks (seq (system/get-hooks context ::authenticate))]
    (some
     (fn [[hook-id hook]]
       (let [res (hook context request)]
        ;;  (system/info context ::authenticate-hook (str hook-id) res) ;; TODO too much output
         res))
     hooks)
    context))

(defn register-authorize-hook [context hook-fn-var]
  (system/register-hook context ::authorize hook-fn-var hook-fn-var))

(defn authorize-hooks [context]
  (system/get-hooks context ::authorize))

;; authenticate hooks should return bool
(defn authorize [context request]
  (or (:public request)
      (->> (authorize-hooks context)
           (some
            (fn [[hook-id hook]]
              (let [res (hook context request)]
                (system/info context ::authorize-hook (str hook-id) res)
                res))))))

(defn register-handle-anonymous-hook [context hook-fn-var]
  (system/register-hook context :http/handle-anonymous hook-fn-var hook-fn-var))

;; authenticate hooks should return http response
(defn handle-anonymous [context request]
  (->> (system/get-hooks context ::handle-anonymous)
       (some
        (fn [[hook-id hook]]
          (let [res (hook context request)]
            (system/info context ::handle-anonymous-hook (str hook-id) res)
            res)))))

;; notify all subscribers on request - can be used for analytics etc

(def REQUEST_SUBS_KEY :on-request)

(defn subscribe-to-request
  "subscribe to requests register (fn [context request opts])"
  [context {f :fn :as opts}]
  (system/set-system-state context [REQUEST_SUBS_KEY f] opts))

(defn unsubscribe-from-request   [context {f :fn}]
  (system/clear-system-state context [REQUEST_SUBS_KEY f]))

(defn request-subscriptions [context]
  (system/get-system-state context [REQUEST_SUBS_KEY]))

(defn notify-on-request [context request]
  (when-let [subs (request-subscriptions context)]
    (future
      (doseq [[f f-opts] subs]
        (f context request  f-opts)))))


(def REQUEST_HOOK_KEY :request-hooks)

(defn register-request-hook [context {f :fn :as opts}]
  (assert (var? f))
  (system/info context ::register-request-hook (str f))
  (system/set-system-state context [REQUEST_HOOK_KEY f] opts))

(defn unregister-request-hook [context {f :fn :as opts}]
  (assert (var? f))
  (system/clear-system-state context [REQUEST_HOOK_KEY f]))

(defn request-hooks [context]
  (system/get-system-state context [REQUEST_HOOK_KEY]))

(defn handle-request-hooks [context request]
  (->> (request-hooks context)
       (reduce (fn [request [_ {f :fn :as opts}]]
                 (f context request opts))
               request)))

(def RESPONSE_HOOK_KEY :response-hooks)

(defn register-response-hook [context {f :fn :as opts}]
  (assert (var? f))
  (system/info context ::register-response-hook (str f))
  (system/set-system-state context [RESPONSE_HOOK_KEY f] opts))

(defn unregister-response-hook [context {f :fn :as opts}]
  (assert (var? f))
  (system/clear-system-state context [RESPONSE_HOOK_KEY f]))

(defn response-hooks [context]
  (system/get-system-state context [RESPONSE_HOOK_KEY]))

(defn handle-response-hooks [context request response]
  (->> (response-hooks context)
       (reduce (fn [response [_ {f :fn :as opts}]]
                 (f context request response opts))
               response)))

(defn dispatch [system {meth :request-method uri :uri :as req}]
  (let [ctx (system/new-context system {::uri uri ::method meth ::remote-addr (:remote-addr req)})
        ctx (apply-middlewares ctx req)]
    (cond
      (and (contains? #{:get :head} meth) (str/starts-with? (or uri "") "/static/"))
      (handle-static req)

      :else
      (let [query-params (parse-params (:query-string req))]
        (if-let [{{f :fn :as op} :match params :params} (resolve-endpoint ctx meth uri)]
          (let [enriched-req (-> (merge req op)
                                 (assoc :query-params query-params :route-params params)
                                 ring.middleware.cookies/cookies-request)
                enriched-req (handle-request-hooks ctx enriched-req)
                auth-ctx (authenticate ctx enriched-req)]
            (if-let [anonimous-response (when (and (authentication-enabled? auth-ctx)
                                                   (not (:public op))
                                                   (not (authenticated? auth-ctx)))
                                          (handle-anonymous auth-ctx enriched-req))]
              anonimous-response
              (if (and (authorization-enabled? auth-ctx) (not (authorize auth-ctx enriched-req)))
                {:status 403}
                (let [start (System/nanoTime)]
                  ;; TODO set context for logger
                  (system/info auth-ctx meth uri {:route-params params})
                  (on-request-hooks auth-ctx {:uri uri :method meth :query-params query-params})
                  (let [res (->> (f auth-ctx enriched-req)
                                 (format-response auth-ctx)
                                 (handle-response-hooks auth-ctx enriched-req)
                                 (ring.middleware.cookies/cookies-response))
                        duration (/ (- (System/nanoTime) start) 1000000.0)]
                    (system/info auth-ctx meth uri {:duration duration  :status (:status res)})
                    (notify-on-request auth-ctx (assoc enriched-req :duration duration))
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

(defn make-pool [queue-capacity min-thread-count max-thread-count]
  (let [ptf   (org.httpkit.PrefixThreadFactory. "worker-")
        queue (java.util.concurrent.ArrayBlockingQueue. queue-capacity)]
    (java.util.concurrent.ThreadPoolExecutor.
     (long min-thread-count)
     (long max-thread-count)
     0
     java.util.concurrent.TimeUnit/MILLISECONDS
     queue
     ptf)))

(system/defmanifest
  {:description "http server module"
   :deps []
   :define-hook {::authorize {:args [::operation-definition ::request] :result ::authorized}}
   :config {:binding {:type "string" :default "127.0.0.1" :validator (complement empty?)}
            :port {:type "integer" :default 8080 :required true :validator pos-int?}
            :max-body {:type "integer" :default 108388608 :required true :validator pos-int?}
            :enable-authorization {:type "boolean"}
            :enable-authentication {:type "boolean"}
            :queue-capacity {:type "integer" :default 20480 :required false :validator pos-int?}
            :min-thread-count {:type "integer" :default 4 :required false :validator pos-int?}
            :max-thread-count {:type "integer" :default 8 :required false :validator pos-int?}}})

(system/defstart [context config]
  (let [{:keys [binding port queue-capacity min-thread-count max-thread-count]} config
        pool (make-pool queue-capacity min-thread-count max-thread-count)
        http-kit-config (assoc (select-keys config [:port :max-body]) :worker-pool pool)]
    (system/info context ::config config)
    (system/info context ::http-kit-config http-kit-config)
    ;; TODO: move to manifest
    (system/manifest-hook context ::on-request {:desc "This hook is called on request and passed method, uri and params"})
    {:server (server/run-server (fn [req] (#'dispatch context req)) http-kit-config)
     :worker-pool pool
     :binding binding
     :port port}))

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
