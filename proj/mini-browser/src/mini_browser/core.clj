(ns mini-browser.core
  (:use compojure.core)
  (:use hiccup.core)
  (:use hiccup.page-helpers)
  (:use ring.adapter.jetty)
  (:require [compojure.route :as route]))
  
;;-----------------------------------------------------------------------------
;; mini-browser lab from labrepl
;;-----------------------------------------------------------------------------
(def default-stylesheets
  ["../../../stylesheets/shCore.css"
   "../../../stylesheets/shThemeDefault.css"
   "../../../stylesheets/application.css"])

(def default-javascripts
  ["../../../javascripts/jquery.js"
   "../../../javascripts/application.js"
   "../../../javascripts/shCore.js"
   "../../../jjavascripts/shBrushClojure.js"])
 
(html [:p "hello world"])
(html [:p {:class "awesome"} "hello world"])

(def title "JF-Browser")

(defn mockup-1
  "test hiccup to html"
  []
  (html 
    [:head 
     [:title title]]
    [:body {:id "browser"} 
     [:p {:class "awesome"} "hello world"]]))

(defn mockup-2 []
  (html
    [:head
     [:title title]]
    [:body {:id "browser"}
     [:div {:id "header"}
      [:h2 title]]
     [:div {:id "content"}
	      "Body"]
     [:div {:id "footer"}
      "Clojure Mini-Browser"]]))

(defn mockup-3 []
  (html
    [:head
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body {:id "browser"}
     [:div {:id "header"}
      [:h2 title]]
     [:div {:id "content"}
      "Body TBD"]
     [:div {:id "footer"}
      "Clojure Mini-Browser"]]))

(defn index-1
  "index.html"
  []
  (html
    [:head 
     [:title title]]
    [:body {:id "browser"} 
     [:p {:class "awesome"} "proceed to m1"]]))

(defroutes mockup-routes 
  (GET "/m1" [] (mockup-1))
  (GET "/m2" [] (mockup-2))
  (GET "/m3" [] (mockup-3))
  (GET "/" [] (index-1))
  (GET "/*" {params :params} (or (serve-file "./" (params :*)) :next)))

(defn mockup-server
  "mockup web server via compojure"
  []
  (run-jetty mockup-routes {:port 8999
                            :join? false}))

;(mockup-server)
;Create a mockup-server function that calls run-server with three arguments:
;   1. a map with the port to use (8999)
;   2. the routes to serve ("/*", i.e. everything)
;   3. a servlet that serves the mockup-routes