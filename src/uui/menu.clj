(ns uui.menu
  (:require
   [uui.heroicons :as ico]
   [system]
   [http]))

;; collect menu from endpoints labeled with :uuid.menu/title

(defn on-endpoint [context endpoint opts]
  (when (::title endpoint)
    (system/set-system-state context [::menu (:path endpoint)] endpoint)))

(defn menu-items [context]
  (system/get-system-state context [::menu]))

(defn init [context]
  (http/subscribe-to-endpoint-register context {:fn #'on-endpoint}))

(defn search-icon [& [cls]]
  [:svg {:class (or cls "size-6") :xmlns "http://www.w3.org/2000/svg" :fill "none" :viewBox "0 0 24 24" :stroke-width "1.5" :stroke "currentColor"} [:path {:stroke-linecap "round" :stroke-linejoin "round" :d "m21 21-5.197-5.197m0 0A7.5 7.5 0 1 0 5.196 5.196a7.5 7.5 0 0 0 10.607 10.607Z"}]])

(defn menu-dialog [context request opts]
  [:dialog#main-menu {:class ["bg-white rounded-lg shadow-xl h-[96vh] w-[96vw] p-6"]
                      :style "margin: 2em; outline: none;"}
   [:div {:class "grid grid-cols-1"}
    [:input
     {:class "border border-gray-300 col-start-1 row-start-1 block w-full rounded-xl py-1.5 pr-3 pl-10 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:pl-9 sm:text-sm/6"
      :type "search"
      :name "search",}]
    (search-icon "size-5 pointer-events-none col-start-1 row-start-1 ml-3 size-5 self-center text-gray-400 sm:size-4 ")]

   [:br]
   (for [[path endpoint] (uui.menu/menu-items context)]
     [:a {:href path
          :class "block px-6 py-2 text-sm font-medium text-sky-700 hover:bg-gray-100 cursor-pointer", :aria-current "page"}
      (:uui.menu/title endpoint)])])

(defn menu-button [context request]
  [:div
   [:a {:class "block px-4 py-4 hover:text-sky-600 cursor-pointer"
        :onclick "document.getElementById('main-menu').showModal()"}
    (ico/bars-3 "size-6")]
   (menu-dialog context request {})])
