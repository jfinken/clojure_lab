(defproject web_crawl_client "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                  [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                  [http.async.client "0.2.0"]]
                  ;[clojure-csv "1.2.0"]]
  :aot [web_crawl_client.core]
  :main web_crawl_client.core)
