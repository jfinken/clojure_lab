(ns web_crawl_client.core
    (:gen-class)
    (:require [http.async.client :as ahc]))

(defn process-line [acc line]
  (+ acc 1))

(defn -main [& args]
  (println (count args)))

;(let [response (ahc/GET "www.nytimes.com")]
;    (ahc/await response)
;    (println (ahc/string response))))


