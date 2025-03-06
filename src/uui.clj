(ns uui
  (:require [system]
            [http]
            [hiccup2.core :as hic]
            [uui.menu]
            [uui.heroicons :as ico]
            [clojure.string :as str]
            [clj-yaml.core]
            [cheshire.core]))

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
    (str "/uui/rpc?" (http/encode-params (assoc params :method fn-nm)))))

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
    [:link {:rel "stylesheet", :href "/static/app.build.css"}]
    [:meta {:name "htmx-config", :content "{\"scrollIntoViewOnBoost\":false}"}]]
   [:body.text-gray-600 {:hx-boost "true"} body ]])

(defn boost-response [context request body]
  (response (if (hx-target request) body (document context request body))))

(def menu-button uui.menu/menu-button)

(defn layout [context request body]
  (boost-response
   context request
   [:div {:class "flex"}
    (menu-button context request)
    [:div {:class "px-6 py-4 flex-1"} body]]))


(defn watch-styles []
  ;; generate app.css
  ;; generate tailwind config
  ;; run tailwindcss -i ./resources/public/app.css -o ./resources/public/app.build.css --watch

  )


(defn tabs [{{tab :tab} :query-params} & tabs]
  (let [tab-pairs (partition 2 tabs)
        tab-index (apply hash-map tabs)
        current-tab (or tab (first tabs))]
    [:div#tab {:class "mb-2"}
     [:div {:class "flex space-x-4 border-b border-gray-300"}
      (for [[tab-name _] tab-pairs]
        [:a
         {:href (str "?tab=" (name tab-name))
          :hx-target "#tab"
          :class  [(if (= current-tab tab-name) "border-sky-500 text-gray-900" "border-transparent")
                   "whitespace-nowrap border-b-2 px-1 pb-1 pt-2 text-sm font-medium text-gray-500 hover:border-gray-300 hover:text-gray-700"]}
         tab-name])]
     [:div {:class "mt-4"}
      (if-let [f (get tab-index current-tab)] (f) [:div.text-red-500 (str "No view for " tab)])]]))


(def bc-delim
  [:svg {:class "size-5 shrink-0 text-gray-400", :viewBox "0 0 20 20", :fill "currentColor", :aria-hidden "true", :data-slot "icon"}
   [:path {:fill-rule "evenodd", :d "M8.22 5.22a.75.75 0 0 1 1.06 0l4.25 4.25a.75.75 0 0 1 0 1.06l-4.25 4.25a.75.75 0 0 1-1.06-1.06L11.94 10 8.22 6.28a.75.75 0 0 1 0-1.06Z", :clip-rule "evenodd"}]])

(defn breadcramp
  "
  (breadcramp [\"url1\" \"label\"] [\"url2\" \"label\"] [\"#\" \"label\"]])
  "
  [& pairs]
  [:ol {:role "list", :class "flex space-x-4 items-center mb-2"}
   (->> pairs
        (map-indexed
         (fn [idx [href & cnt]]
           (into
            [:a {:href href :class "text-sm font-medium text-gray-500 hover:text-gray-700 flex items-center hover:text-sky-600"}
             (when (= 0 idx)
               (ico/home "mr-2 size-4 text-gray-400"))
             [:div cnt]])))
        (interpose (ico/chevron-right "size-4 text-gray-400")))])

(defn json-block [data]
  [:pre.uui-code
   (raw
    (-> (cheshire.core/generate-string data {:pretty true})
        (str/replace #"\"(.*)\" :" "<span class='text-gray-400'>\"</span><b class='font-semibold text-gray-500'>$1</b><span class='text-gray-400'>\"</span><span class='text-gray-400'>:</span>")
        (str/replace #"," "<span class='text-gray-300'>,</span>")
        ))])

(defn yaml-block [data]
  [:pre.uui-code
   (raw
    (-> (clj-yaml.core/generate-string data)
        (str/replace #"(?m)^(.*):" "<b class='font-semibold text-gray-500'>$1</b><span class='text-gray-400'>:</span>")
        ))])

(comment


  )
