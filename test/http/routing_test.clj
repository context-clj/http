(ns http.routing-test
  (:require [clojure.test :refer :all]
            [http.routing :as rm]
            [matcho.core :as matcho]
            [clojure.string :as str]))

(deftest test-pathify
  (is (rm/pathify "/") [])
  (is (rm/pathify "/users/") ["users"])
  (is (rm/pathify "users/") ["users"])
  (is (rm/pathify "users") ["users"])
  (is (rm/pathify "users/1/show") ["users" "1" "show"]))


;; DSL example
(defn resource [nm & [chld]]
  "DSL example"
  {nm {:get {:fn 'index}
       :post {:fn 'create}
       [(keyword (str nm "_id"))]
       (merge
         {:get {:fn 'show}
          :put {:fn 'update}
          :DELETE {:fn 'delete}} (or chld {})) }})


(def meta-routes
  {:get ^{:fls [1]} 'root })

(def GET :get)
(def POST :post)
(def PUT :put)

(def user-routes
  {:.name :users
   :.roles   #{:admin}
   GET       {:.desc "List users"}
   POST      {:.desc "Create user" :.roles #{:admin}}
   "active"   {:.name :active-users
               GET {:.desc "Filtering users"}}

   [:user-id] {:.name :user
               GET       {:.desc "Show user"}
               POST      {:.desc "Update user"}
               "activate" {:.name :activate-user
                           POST {:.desc "Activate"}}}})

(def routes
  {:.name :root
   GET    {:.desc "Root"}
   "posts" {:.roles   #{:author :admin}
            :.filters [:user-required]
            GET       {:.desc "List posts"}
            POST      {:.desc "Create post"}
            [:post-id] {GET       {:.desc "Show post"}
                        POST      {:.desc "Update post"}
                        "publish"  {POST {:.desc "Publish post"}}
                        "comments" {GET  {:.desc "comments"}
                                    [:comment-id] {GET {:.desc "show comment"}
                                                   PUT {:.desc "update comment"}}}}}
   "sites" {[:site]  {[:path*] {GET {:.desc "Glob files"}}}}
   "users" #'user-routes})

(defn- get-desc [path]
  (:.desc
    (:match
      (rm/match path routes))))

(defn- get-params [path]
  (:params
    (rm/match path routes)))

(rm/match [:get "users/1"] routes)

#_(time
  (doseq [x (take 100000 (range))]
    (rm/match [:post (str "/users/" x "/activate")] routes)))

(deftest match-routes
  (is (= (rm/match [:post "/"] routes)
         nil))

  (is (= (rm/match [:get "some/unexistiong/route"] routes)
         nil))

  (matcho/match
   (rm/match [:get "users/active"] routes)
   {:match {:.desc "Filtering users"}
    :parents #(= 3 (count %))})

  (matcho/match
   (rm/match [:get "/users/active"] routes)
   {:match {:.desc "Filtering users"}
    :parents #(= 3 (count %))})

  (matcho/match
   (rm/match [:get "/"] routes)
   {:match {:.desc "Root"}})

  (matcho/match
   (rm/match [:post "users/5/activate"] routes)
   {:match {:.desc "Activate"}
    :params {:user-id "5"}
    :path ["users" :user-id "activate" nil?]
    :parents #(= 4 (count %))})


  (is (= (mapv :.filters (:parents (rm/match [:get "posts/1"] routes)))
         [nil [:user-required] nil]))

  (matcho/match (rm/match [:get "sites/blog/imgs/logo.png"] routes)
                {:path ["sites" :site :path* nil?]
                 :params {:site "blog"
                          :path* ["imgs" "logo.png"]}}))

(def routes-2
  {"metadata" {:get {:fn '=metadata}}
   [:type] {:post {:fn '=create}
            [:id] {:get   {:fn '=read}
                   :DELETE  {:fn '=delete} }}})


(deftest empty-root-test
  (is (= (rm/match [:get "/"] routes-2)
         nil)))

(defn guard [x]
  (= "special" (get-in x [:param :params])))

(def routes-specific
  {[:param] {"action" {:get {:fn 'action }}
             :get {:fn 'special :guard guard}}})

(defn match-specific [meth path]
  (get-in (rm/match [meth path] routes-specific) [:match :fn]))

(deftest specila-test
  (is (= (match-specific :get "/special") 'special))
  (is (= (match-specific :get "/special/action") 'action)))

(def frontend-routes
  {"admin" {"users" {:. 'users-list-view
                     [:id] 'user-view}
            "groups" 'groups-list-view}})

(defn f-match [url]
  (:match (rm/match url frontend-routes)))

(deftest frontend-routes-test
  (is (= 'users-list-view (f-match "/admin/users")))

  (matcho/match
   (rm/match "/admin/users/5" frontend-routes)
   {:match 'user-view
    :path ["admin" "users" :id nil?]
    :params {:id "5"}})

  (is (= 'groups-list-view (f-match "/admin/groups")))
  )

(deftest not-map-test
  (is (nil? (rm/match "/test/unexisting" {"test" :test}))))


(defn match-ids [k]
  (when (re-matches #".*,.*" k)
    {:ids (str/split k #",")}))

(match-ids "1")

(match-ids "1,2")

(def fn-params-routes
  {"user" {[:id] {:get 'user}
           [match-ids] {:get 'specific}}})

(deftest frontend-routes-test
  (matcho/match
   (rm/match [:get "/user/1"] fn-params-routes)
   {:match 'user
    :path ["user" :id nil?]
    :params {:id "1"}})

  (matcho/match
   (rm/match [:get "/user/1,2"] fn-params-routes)
   {:match 'specific
    :path ["user" :ids]
    :params {:ids ["1", "2"]}}))

(deftest no-method-glob-test
  (def bits-routes {"page" {[:bits*] 'bits}})
  (matcho/match
   (rm/match "/page/test" bits-routes)
   {:params {:bits* ["test"]}
    :path ["page" :bits*]
    :match 'bits})

  (matcho/match
   (rm/match "/page/test/a/b/c" bits-routes)
   {:params {:bits* ["test" "a" "b" "c"]}
    :match 'bits})

  (def bits-rotes-2 {"page" {[:bits*] {:get 'get-bits
                                       :post 'post-bits}}})

  (matcho/match
   (rm/match [:get "/page/test"] bits-rotes-2)
   {:path ["page" :bits*]
    :params {:bits* ["test"]}})

  (matcho/match
   (rm/match [:get "/page/test/a/b/c"] bits-rotes-2)
   {:path ["page" :bits*]
    :params {:bits* ["test" "a" "b" "c"]}})

  (matcho/match
   (rm/match [:get "/page/test"] bits-rotes-2)
   {:path ["page" :bits*]
    :params {:bits* ["test"]}
    :match 'get-bits})

  )

(def multi-rs
  {[:resource-type] {:get :list
                     [:id] {:get :find
                            :put :update}}
   "AidboxJob" {[:id] {"$run" {:post :post}}}

   "Appointment" {"$op" {:post :op
                         :get  :op}
                  [:id] {"$sub" {:get :sub}}}})

(deftest multi-routes

  (rm/match [:post "/AidboxJob/3/$run"] multi-rs)
  (rm/match [:get "/AidboxJob"] multi-rs)


  (matcho/match
   (rm/match [:get "/Patient/1"] multi-rs)
   {:match :find
    :path [:resource-type :id]
    :params {:resource-type "Patient"
             :id "1"}})

  (matcho/match
   (rm/match [:get "/Appointment/1"] multi-rs)
   {:match :find
    :path [:resource-type :id]
    :params {:resource-type "Appointment"
             :id "1"}})

  (matcho/match
   (rm/match [:get "/Appointment/1"] multi-rs)
   {:match :find
    :path [:resource-type :id]
    :params {:resource-type "Appointment"
             :id "1"}})

  (matcho/match
   (rm/match [:put "/Appointment/1"] multi-rs)
   {:match :update
    :path [:resource-type :id]
    :params {:resource-type "Appointment"
             :id "1"}})

  (matcho/match
   (rm/match [:get "/Appointment/$op"] multi-rs)
   {:match :op
    :path ["Appointment" "$op"]
    })

  (matcho/match
   (rm/match [:get "/Appointment/5/$sub"] multi-rs)
   {:match :sub
    :path ["Appointment" :id "$sub"]
    }))



;; TODO
;; * nested params (full naming or fallback to id)
;; * dsl
;; * meta-info
;; * handler
;; * [:*]
;; * [:param #"regexp"]

(deftest url-test
  (is (= (rm/url routes :root) "/"))
  (is (= (rm/url routes :not-exists) nil))
  (is (= (rm/url routes :posts) "/posts"))
  (is (= (rm/url routes :posts [42]) "/posts/42"))
  (is (= (rm/url routes :posts {:post-id 42}) "/posts/42"))
  (is (= (rm/url routes :posts-comments [42 24]) "/posts/42/comments/24"))
  (is (= (rm/url routes :posts-comments {:comment-id 24 :post-id 42}) "/posts/42/comments/24"))
  (is (= (rm/url routes :active-users) "/users/active"))
  (is (= (rm/url routes :activate-user [111]) "/users/111/activate"))
  (is (= (rm/url routes :activate-user {:user-id 111}) "/users/111/activate")))


(def test-parent-params
  {:context   :ctx-1
   GET        {:.desc "a"}
   [:param-1] {:context :ctx-2
               GET {:.desc "b"}
               "item" {:context :ctx-3
                       GET {:.desc "c"}
                       [:param-3] {GET {:.desc "d"}}}
               [:param-2] {:context :ctx-3
                           GET {:.desc "c"}
                           [:param-3] {GET {:.desc "d"}}}}})

(deftest parent-params-test
  (matcho/match
   (rm/match [:get "/p1/p2/p3"] test-parent-params)
   {:parents [{:params {}}
              {:params {:param-1 "p1"}}
              {:params {:param-1 "p1" :param-2 "p2"}}
              {:params {:param-1 "p1" :param-2 "p2" :param-3 "p3"}}]})

  (matcho/match
   (rm/match [:get "/p1/item/p3"] test-parent-params)
   {:parents [{:params {}}
              {:params {:param-1 "p1"}}
              {:params {:param-1 "p1"}}
              {:params {:param-1 "p1" :param-3 "p3"}}]}))

(def test-param-constraints
  {[:entity ] {:route-map/enum #{"User" "Admin"}
               GET :a
               "sub" {GET :x}}
   "Admin" {GET :z}
   [:pattern] {:route-map/regexp #"^prefix_"
               :get :pat}
   [:default] {GET :b
               "sub" {"subsub" {GET :y}}}})

(def test-param-corner-case
  {[:entity ] {:route-map/enum #{"User"}
               GET :a}
   "Admin" {GET :z}})

(deftest param-constraints-test
  (matcho/match
   (rm/match [:get "/User"] test-param-constraints)
   {:match :a
    :params {:entity "User"}})

  (matcho/match
   (rm/match [:get "/Admin"] test-param-constraints)
   {:match :z})

  (matcho/match
   (rm/match [:get "/Admin/sub"] test-param-constraints)
   {:match :x})

  (matcho/match
   (rm/match [:get "/Admin/sub/subsub"] test-param-constraints)
   {:match :y})

  (matcho/match
   (rm/match [:get "/Patient"] test-param-constraints)
   {:match :b
    :params {:default "Patient"}})

  (matcho/match
   (dissoc (rm/match [:get "/prefix_something"] test-param-constraints) :parents)

   {:match :pat
    :params {:pattern "prefix_something"}})

  (testing "when no match with params"
    (matcho/match
     (dissoc (rm/match [:get "/Admin"] test-param-corner-case) :parents)

     {:match :z})

    (matcho/match
     (dissoc (rm/match [:get "/User"] test-param-corner-case) :parents)

     {:match :a})

    (is (nil? (rm/match [:get "/Ups"] test-param-corner-case)))


    )

  )
