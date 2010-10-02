(ns drawing-demo
  (:import [javax.swing JPanel JFrame]
           [java.awt Dimension]))
 
(defn make-panel []
  (let [panel (proxy [JPanel] []
                (paintComponent [g]
                  (.drawLine g 0 0 100 100)))]
    (doto panel
      (.setPreferredSize (Dimension. 400 400)))))
 
(defn make-frame [panel]
  (doto (new JFrame)
    (.add panel)
    .pack
    .show))
 
(make-frame (make-panel))

