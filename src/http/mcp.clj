(ns http.mcp
  (:require
   [org.httpkit.server :as http-kit]
   [http]
   [system]
   [cheshire.core :as cheshire]
   [clojure.string :as str]))

(system/defmanifest
  {:description "MCP server"
   :dependencies []
   :config {:name      {:type "string" :required true}
            :version   {:type "string" :default "0.0.1"}
            :instructions {:type "string"}
            :protocol-version {:type "string" :default "2024-11-05"}}})

(def current-namespace *ns*)

(defn channel-send [channel event data]
  (http-kit/send!
   channel
   {:status 200
    :headers {"content-type" "text/event-stream" "cache-control" "no-cache, no-store"}
    :body   (str "event: " event "\n" "data: " data "\n\n")} false))

(defn ^{:http {:path "mcp" :method :get}} mcp
  [context request]
  (let [client-id (str (java.util.UUID/randomUUID))]
    (http-kit/with-channel request channel
      (println :mcp client-id channel)
      (http-kit/on-close channel (fn [status]
                                   (println :close channel status)
                                   (system/update-system-state context [:mcp-clients] (fn [x] (dissoc x client-id)))))
      (system/set-system-state context [:mcp-clients client-id] channel)
      (channel-send channel "endpoint" (str "/mcp/" client-id "/messages"))
      {:status 200 :headers {"Content-Type" "text/event-stream"}})))

(defmulti mcp-handler (fn [context request] (:method request)))

(defmethod mcp-handler :default 
  [context request]
  {:error {:message (str "Unknown method: " (:method request))}})

(defmethod mcp-handler "initialize"
  [context request]
  (println :mcp-initialize request)
  {:result 
   {:protocolVersion (system/get-config context :protocol-version)
    :capabilities {:tools {:listChanged false}}
    :serverInfo {:name (system/get-config context :name) :version (system/get-config context :version)}
    :instructions (system/get-config context :instructions)}})

(defmethod mcp-handler "notifications/initialized"
  [context request]
  (println :mcp-initialized request)
  {:result {}})

(defn register-tool [context {f :fn n :name  :as opts}]
  (assert n ":name is required")
  (assert (and f (var? f)) ":fn is required")
  (system/info context ::register-tool opts)
  (system/set-system-state context [:tools n] opts))

(defn unregister-tool [ctx {name :name}]
  (system/clear-system-state ctx [:tools name]))

(defn collect-tools-from-ns [ns]
  (->> (ns-interns ns)
       (filter #(:mcp/tool (meta (second %))))
       (remove nil?)
       (mapv (fn [[nm v]]
               (let [vm (meta v)
                     m (:mcp/tool vm)]
                 (merge {:fn v :name (str (clojure.string/replace (:ns vm) "." "_") "__" nm)} m))))))


(defn register-ns-tools [context ns]
  (doseq [tool (collect-tools-from-ns ns)]
    (register-tool context tool)))

(defn unregister-ns-tools [context ns]
  (doseq [tool (collect-tools-from-ns ns)]
    (system/info context ::unregister-tool tool)
    (unregister-tool context tool)))


(defn get-tools [context]
  (->> (vals (system/get-system-state context [:tools]))
       (map (fn [tool] (dissoc tool :fn)))
       (sort-by :name)))

(defmethod mcp-handler "tools/list"
  [context request]
  (println :mcp-tools-list request)
  {:result {:tools (get-tools context)}})

(defmethod mcp-handler "tools/call"
  [context request]
  (println :mcp-tools-call request)
  (if-let [handler (system/get-system-state context [:tools (get-in request [:params :name]) :fn])]
    (let [result (handler context (get-in request [:params :arguments]))]
      {:result {:content [{:type "text" :text (cheshire.core/generate-string result)}]}})
    {:error {:message (str "Unknown tool: " (:name request))}}))

(defn ^{:http {:path "/mcp/:client/messages" :method :post}}
  mcp-messages [context request]
  (let [client-id (get-in request [:route-params :client])
        channel (system/get-system-state context [:mcp-clients client-id])
        req (cheshire.core/parse-string (slurp (:body request)) keyword)
        result (mcp-handler context req)]
    (println :mcp> client-id req)
    (when-let [id (:id req)]
      (let [resp (cheshire.core/generate-string (merge {:jsonrpc "2.0" :id id} result))]
        (println :mcp< (cheshire.core/generate-string result))
        (channel-send channel "message" resp)))
    {:status 200}))

(defn get-channels [context]
  (system/get-system-state context [:mcp-clients]))

(system/defstart
  [context config]
  (http/register-ns-endpoints context current-namespace)
  {})

(system/defstop
  [context state])

(comment
  (def context far/context)

  (get-channels context)
  (get-tools context)

  (system/get-system-state context [:tools])

  )


