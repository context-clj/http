(ns uui
  (:require [system]
            [http]
            [hiccup2.core :as hic]
            [uui.menu]
            [clojure.string :as str]))

(def raw hic/raw)

(system/defmanifest {:description "Universal UI module"})

(defn hiccup [content]
  (str (hiccup2.core/html content)))

(defn response [body]
  {:status 200
   :headers {"content-type" "text/html"}
   :body (hiccup body)})

(defn rpc [fn-name & [params]]
  (let [m (meta fn-name)
        fn-nm  (str (:ns m) "/" (:name m))]
    (str "/ui/rpc?" (http/encode-params (assoc params :method fn-nm)))))

(defn get-fn [fn-str]
  (let [[ns-name _fn-name] (str/split fn-str #"/")]
    (require [(symbol ns-name)])
    (resolve (symbol fn-str))))

;; TODO: check meta - security whole
(defn ui-rpc [context request]
  (let [m (:method (:query-params request))
        f (get-fn m)
        res (f context request (:query-params request))]
    (system/info context ::rpc m)
    (if (vector? res) (response res) res)))

(defn init [context]
  (uui.menu/init context))

(defn mount-routes [context]
  (http/register-endpoint context {:method :get :path  "/uui/rpc"  :fn #'ui-rpc})
  (http/register-endpoint context {:method :post :path  "/uui/rpc"  :fn #'ui-rpc})
  (http/register-endpoint context {:method :delete :path  "/uui/rpc"  :fn #'ui-rpc})
  (http/register-endpoint context {:method :put :path  "/uui/rpc"  :fn #'ui-rpc}))

(system/defstart [context config]
  (init context)
  (mount-routes context))

(defn script [& cnt]
  [:script (raw (str/join cnt))])

(defn hx-target [request]
  (get-in request [:headers "hx-target"]))

(defn document [context request body]
  [:html
   [:head
    [:script {:src "/static/htmx.min.js"}]
    [:meta {:name "htmx-config", :content "{\"scrollIntoViewOnBoost\":false}"}]]
   [:body.text-gray-600 {:hx-boost "true"} body ]])

(defn boost-response [context request body]
  (response (if (hx-target request) body (document context request body))))

(def menu-button uui.menu/menu-button)


(comment







  )
