(ns http.formats
  (:require
    [cheshire.core :as json]
    [cheshire.parse :as json-parse]
    [cheshire.generate :as json-gen]
    [clj-yaml.core :as yaml]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [clojure.walk]
    [cognitect.transit :as transit]
    [ring.middleware.multipart-params :as multi]
    [ring.middleware.multipart-params.byte-array :as ba]
    [ring.util.codec]
    [ring.util.io])
  (:import
    (clojure.lang Keyword Symbol Var)
    (com.fasterxml.jackson.core JsonGenerator JsonParseException) 
    (java.io BufferedWriter ByteArrayInputStream ByteArrayOutputStream InputStream OutputStreamWriter PushbackReader)
    (java.util ArrayList LinkedHashMap LinkedHashSet) (org.httpkit.server AsyncChannel)))

(extend-protocol yaml/YAMLCodec
  LinkedHashMap
  (clj-yaml.core/decode [data keywords]
    (letfn [(decode-key
              [k]
              (if keywords
                ;; (keyword k) is nil for numbers etc
                (or (keyword k) k)
                k))]
      (into {}
            (for [[k v] data]
              [(-> k (yaml/decode keywords) decode-key) (yaml/decode v keywords)]))))

  LinkedHashSet
  (clj-yaml.core/decode [data _keywords]
    (into #{} data))

  ArrayList
  (clj-yaml.core/decode [data keywords]
    (mapv #(yaml/decode % keywords) data)))


(json-gen/add-encoder AsyncChannel json-gen/encode-str)
(json-gen/add-encoder Var json-gen/encode-str)
(json-gen/add-encoder Object json-gen/encode-str)


(extend-protocol yaml/YAMLCodec
  java.time.LocalDate
  (encode [d] (str d))

  java.time.ZonedDateTime
  (encode [d] (str d))

  Keyword
  (encode [data]
    (str/join "/" (remove nil? [(namespace data) (name data)])))
  Symbol
  (encode [data]
    (str/join "/" (remove nil? [(namespace data) (name data)]))))


(defmulti do-format (fn [fmt _ _pretty?] fmt))


(defmethod do-format :json [_ body pretty?]
  (binding [json-parse/*use-bigdecimals?* true]
   (json/generate-string body {:pretty pretty?})))


(defmethod do-format :js [_ body pretty?]
  (str "var AIDBOX_DATA = "
       (json/generate-string body {:pretty pretty?})
       ";"))


(defmethod do-format :yaml [_ body _]                       ; (yaml always pretty)
  (yaml/generate-string body))


(defmethod do-format :text [_ body _] body)


(defmethod do-format :edn [_ body pretty?]
  (if pretty?
    (with-out-str (pprint/pprint body))
    (with-out-str (pr body))))


(defmethod do-format :transit [_ body _]                    ; (transit always ugly)
  (ring.util.io/piped-input-stream
    (fn [out] (transit/write (transit/writer out :json) body))))


(defmulti parse-format (fn [fmt _ _] fmt))


(defn parse-json
  [content]
  (when content
    (binding [json-parse/*use-bigdecimals?* true]
      {:resource (cond
                   (string? content) (json/parse-string content keyword)
                   (instance? InputStream content) (json/parse-stream (io/reader content) keyword)
                   :else content)})))

(defmethod parse-format :json [_ _ {b :body}]
  (parse-json b))


(defmethod parse-format :edn [_ _ {b :body}]
  (when b
    {:resource
     (cond
       (string? b) (edn/read-string b)
       (instance? InputStream b)
       (-> b
           io/reader
           PushbackReader.
           edn/read)
       :else b)}))


(defmethod parse-format :transit [_ _ {b :body}]
  (when b
    (let [r (cond (string? b) (transit/reader (ByteArrayInputStream. (.getBytes ^String b)) :json)
                  (instance? InputStream b) (transit/reader b :json))]
      {:resource (transit/read r)})))


(defmethod parse-format :form-data [_ _ req]
  {:form-params (clojure.walk/keywordize-keys (:multipart-params (multi/multipart-params-request req {:store (ba/byte-array-store)})))})


(defmethod parse-format :text [_ct _ {^String b :body}]
  (when b
    {:text
     (cond
       (string? b) b
       (instance? InputStream b)
       (slurp b :encoding "UTF-8"))}))


(defmethod parse-format :ndjson [_ct _ {b :body}]
  (when b
    {:body b}))

(defmethod parse-format nil [_ct _ {b :body}]
  (try (parse-json b)
       (catch JsonParseException e
         (throw (ex-info (format "`content-type` header is missing, body parsed as JSON by default and got this error:\n%s\nProvide appropriate content-type for your body or valid JSON" (ex-message e)) {})))))


(defmethod parse-format :default [ct _ _]
  (throw (RuntimeException. (str "Unknown/not supported Content-Type: " ct))))


(defmethod parse-format :yaml [_ _ {b :body}]
  (when b
    {:resource (cond
                 (string? b) (yaml/parse-string b)
                 (instance? InputStream b)
                 (let [b (slurp b)]
                   (yaml/parse-string b))
                 :else b)}))

(defmethod parse-format :cda [_ _ {b :body}]
  (when b
    (let [dom (if (string? b) b (slurp b))]
      {:resource dom
       :body     dom})))


(defn form-decode
  [s]
  (clojure.walk/keywordize-keys (ring.util.codec/form-decode s)))


(defmethod parse-format :query-string [_ _ {b :body}]
  (when-let [b (cond
                 (string? b) b
                 (instance? InputStream b)
                 (if (pos? (.available ^InputStream b))
                   (slurp (io/reader b))
                   nil)
                 :else nil)]
    {:form-string b
     :form-params (form-decode b)}))


(def ct-mappings
  {"application/json"                  :json
   "application/json+fhir"             :json
   "application/fhir+json"             :json
   "application/json-patch+json"       :json
   "application/merge-patch+json"      :json
   "application/scim+json"             :json
   "application/fhir+ndjson"           :ndjson
   "application/ndjson"                :ndjson
   "application/x-ndjson"              :ndjson

   "application/transit+json"          :transit
   "text/yaml"                         :yaml
   "text/edn"                          :edn
   "text/plain"                        :text
   "text/html"                         :text
   "*/*"                               :json
   "application/x-www-form-urlencoded" :query-string
   "multipart/form-data"               :form-data
   "application/yaml"                  :yaml
   "application/edn"                   :edn

   "application/cda+xml"               :cda})


(defn header-to-format
  [content-type]
  (if (str/blank? content-type)
    [nil ""]

    (let [[ct options] (->> (str/split (first (str/split content-type #",")) #"\s*;\s*")
                            (map str/trim))]
      [(get ct-mappings ct ct) options])))


(defn parse-accept-header
  [ct]
  (map str/trim (str/split ct #"[,;]")))


(defn get-format
  [ct]
  (if-let [fmt (ct-mappings ct)]
    fmt
    (when (str/starts-with? ct "charset")
      :charset)))


(defn append-charset
  [[fmt ac] charset]
  [fmt (str ac \; charset)])


(defn select-accept-header
  "Prioritizes json if possible."
  [ct]
  (when (and ct (string? ct))
    (let [{json-cts      :json
           [[_ charset]] :charset
           other-cts     nil} (->> (parse-accept-header ct)
                                   (map (fn [ct] [(get-format ct) ct]))
                                   (filter first)
                                   (group-by (comp #{:json :charset} first)))]
      (cond-> (or (first json-cts) (first other-cts))
        charset (append-charset charset)))))


(defn accept-header-to-format
  [ct]
  (first (select-accept-header ct)))


(defn content-type
  [fmt accept]
  (let [[selected-fmt selected-accept] (select-accept-header accept)]
    (if (and (= fmt selected-fmt)
             (not= "*/*" selected-accept))
      selected-accept
      (get {:edn     "text/edn"
            :json    "application/json"
            :transit "application/transit+json"
            :js      "text/javascript"
            :yaml    "text/yaml"} fmt))))


(def known-formats
  {"json"    :json
   "edn"     :edn
   "cda"     :cda
   "js"      :js
   "transit" :transit
   "yaml"    :yaml})

(def default-response-format :json)

(defn known-response-format? [fmt]
  (contains? (methods do-format) fmt))

(defn get-format-from-headers
  [{{ac "accept", ct "content-type"} :headers :as _request}]
  (let [fmt (cond
              ac (accept-header-to-format ac)
              ct (accept-header-to-format ct))]
    (if (known-response-format? fmt) fmt nil)))

(defn get-wanted-format
  [{{fmt :_format _pretty? :_pretty} :params :as request}]
  (let [fmt-from-param
        (or (get known-formats fmt)
            (get ct-mappings fmt))

        fmt-from-headers
        (get-format-from-headers request)

        wanted-fmt
        (cond
          fmt fmt-from-param
          fmt-from-headers fmt-from-headers
          :else default-response-format)]
    (cond
      (nil? wanted-fmt) nil
      (known-response-format? wanted-fmt) wanted-fmt
      :else default-response-format)))

(defn add-body-bytes
  [config {:as req :keys [body]}]
  (cond
    (not (get config :request-save-raw-body))
    req

    (string? body) (assoc req :body-bytes (bytes (byte-array (map (comp byte int) body))))

    (instance? InputStream body)
    (let [body-byte-array-output-stream (ByteArrayOutputStream.)
          _ (.transferTo body body-byte-array-output-stream)
          body-bytes (.toByteArray body-byte-array-output-stream)]
      (assoc req
             :body (ByteArrayInputStream. body-bytes)
             :body-bytes body-bytes))

    :else req))


(defn parse-body
  [{body :body {ct "content-type"} :headers {_fmt :_format} :params :as req} & [config]]
  (let [[current-content-type options] (header-to-format ct)]
    (when body
      (let [req-with-body-bytes (add-body-bytes config req)
            parsed-body (parse-format current-content-type options req-with-body-bytes)]
        (merge req-with-body-bytes parsed-body)))))
