(ns mini-browser.core
  (:use compojure.core)
  (:use hiccup.core)
  (:use hiccup.page-helpers)
  (:use ring.adapter.jetty)
  (:use ring.middleware.reload)
  (:use ring.middleware.stacktrace)
  (:use ring.middleware.file)
  (:use ring.middleware.file-info)
  (:use mini-browser.util)  
  (:require [compojure.route :as route]
            [clojure.string :as str]
            [clojure.repl :as repl]))
  
;;-----------------------------------------------------------------------------
;; mini-browser lab from labrepl
;;
;; also, great post on end-to-end web app dev and delopyment
;; mmcgrana.github.com/2010/07/develop-deploy-clojure-web-applications.html
;;-----------------------------------------------------------------------------
(def default-stylesheets
  ["/stylesheets/shCore.css"
   "/stylesheets/shThemeDefault.css"
   "/stylesheets/application.css"])

(def default-stylesheets-1
  ["/public/shCore.css"
   "/public/shThemeDefault.css"
   "/public/application.css"])

(def default-javascripts
  ["/javascripts/jquery.js"
   "/javascripts/application.js"
   "/javascripts/shCore.js"
   "/javascripts/shBrushClojure.js"])
 
(html [:p "hello world"])
(html [:p {:class "awesome"} "hello world"])

(def title "Mini-Browser")

(defn mockup-1
  "test hiccup to html"
  []
  (html 
    [:head 
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body {:id "browser"} 
     [:p {:class "awesome"} "hello world"]]))

(defn mockup-2 []
  (html
    [:head
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
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
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body {:id "browser"} 
     [:p {:class "awesome"} "proceed to m1"]]))

(defn namespace-link
  "returns an anchor tag linking to the passed in namespace name."
  [ns-name]
  [:a {:href (str "/browse/" ns-name)} ns-name])

(defn namespace-browser
  "takes a collection of namespace names, and creates a div.browse-list 
around an unordered list of links from namespace-link."
  [ns-names]
  [:div
   {:class "browse-list"}
   [:ul
    (map
     (fn [ns] [:li (namespace-link ns)])
     ns-names)]])

(defn var-link
  "takes a namespace name and a var name and builds a link."
  [ns-name var-name]
  [:a {:href (str "/browse/" ns-name "/" (java.net.URLEncoder/encode (str var-name)))} var-name])

(defn var-browser
  "puts a div.browse-list variables around an unorderd list of var links"
  [ns var-links]
  (html
    [:div
     {:class "browse-list variables"}
     [:ul
      (map
        (fn [v] [:li (var-link ns v)])
        var-links)]]))

(defn mockup-4 []
  (html
    [:head
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body {:id "browser"}
     [:div {:id "header"}
      [:h2 title]]
     [:div {:id "content"}
      (namespace-browser ["fake-ns1" "fake-ns2"])
      (var-browser "fake-ns1" ["some-var-1" "some-var-2"])]
     [:div {:id "footer"}
      "Clojure Mini-Browser"]]))

(defroutes mockup-routes 
  (GET "/m1" [] (mockup-1))
  (GET "/m2" [] (mockup-2))
  (GET "/m3" [] (mockup-3))
  (GET "/m4" [] (mockup-4))
  (GET "/*" [] (index-1)))
  
(def app
  (-> #'mockup-routes
    ; serve static files, i.e. js and css, starting at the current root
    (wrap-file "./")
    (wrap-reload '[mini-browser.core])
    (wrap-stacktrace)))

(defn mockup-server
  "mockup web server via compojure"
  []
  (run-jetty #'mini-browser.core/app {:port 8999
                            :join? false}))

;(mockup-server)

;;-----------------------------------------------------------------------------
;; Making it live
;;-----------------------------------------------------------------------------
(defn layout [& body]
  (html
    [:head
     [:title title]
     (apply include-css default-stylesheets)
     (apply include-js default-javascripts)]
    [:body {:id "browser"}
     [:div {:id "header"}
      [:h2 title]]
     [:div {:id "content"}
      body]
     [:div {:id "footer"}
      "Clojure Mini-Browser"]]))

(defn namespace-names
  "return all namespace names sorted, calls all-ns"
  []
  (sort (map ns-name (all-ns))))

(defn var-names
  "Sorted list of var names in a namespace (symbols)."
  [ns]
  (when-let [ns (find-ns (symbol ns))]
    (sort (keys (ns-publics ns)))))

(defn var-symbol
 "Create a var-symbol, given the ns and var names as strings."
  [ns var]
  (symbol (str ns "/" var)))	

(defn view-function
  [func]
  (html
   [:h3 (find-var (symbol func))]))

(defn var-symbol
  "Create a var-symbol, given the ns and var names as strings."
  [ns var]
  (symbol (str ns "/" var)))

(defn var-detail
  [ns var]
  (when var
    (let [sym (var-symbol ns var)
          var (find-var sym)]
      (html [:h3 sym]
            [:h4 "Docstring"]
            [:pre [:code
                   (with-out-str (print-doc var))]]
            [:h4 "Source"]
            (code* (repl/source-fn sym))))))

(defroutes browser-routes
  (GET
   "/"
   []
   (html
    (layout
     (namespace-browser (namespace-names))
     [:div {:class "browse-list empty"}])))
  (GET
   "/browse/*"
   request
   (let [[ns var] (str/split (get-in request [:params "*"]) #"/")]
     (html
      (layout
       (namespace-browser (namespace-names))
       (var-browser ns (var-names ns))
       (var-detail ns var))))))

(defn namespace-names-less-intuitive
  "Sorted list of namespace names (strings)."
  []
  (->> (all-ns)
       (map #(.name %))
       (sort)))

(defroutes static-routes
  "for static content"
  (route/files "/")
  (route/not-found "<h1>Not Found.  Oh snap!  404ed your ass...</h1>"))

(defroutes app-routes
  (routes browser-routes static-routes))

(defn main []
  (run-jetty (var app-routes) {:port 9000
                               :join? false}))