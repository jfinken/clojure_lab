; require contrib.string
(ns dialect
  (:require [clojure.contrib.string :as s])
  (:import
    (java.awt Color Dimension)
    (java.awt.event KeyListener)
    (java.awt.event ActionListener)
    (javax.swing JFrame JOptionPane JPanel BoxLayout JTextField JTextArea)))

(defn canadianize [sentence] (str sentence ", eh"))

; playing with type hinted string - unused here
(defn str-len [^String x]
  (.length x))

(defn pig-latin [word]
  ;take first letter, capitalize, add to end with '-ay'
  (str 
    (s/tail (- (.length word) 1) word)
    "-"
    (s/upper-case (str (get word 0)))
    "ay "))
  
(defn pig-latinize [sentence] 
  (if (= sentence "")
    ""
    (s/chop
      (s/map-str 
        pig-latin    
        (.split sentence " ")))
    ))

; GUI
(defn create-panel [width height]
  (proxy [JPanel KeyListener]
   [] ; superclass constructor arguments
    (getPreferredSize [] (Dimension. width height))
    (keyPressed [e])
    ;  (compare-and-set! key-code-atom @key-code-atom (.getKeyCode e)))
    (keyReleased [e]) ; do nothing
    (keyTyped [e]) ; do nothing
  ))
"
(defn create-panel [width height]
  (JPanel.))
"
(defn create-text-field [width]
  (JTextField. width))

(defn create-text-area [rows cols]
  (JTextArea. rows cols))

(defn configure-gui [frame panel text-field text-area]
  ; make text-area non-editable
  (.setEditable text-area false)
  ; event listener for text-field
  (.addActionListener text-field
    (proxy [ActionListener][]
      (actionPerformed[evt]
        (.setText text-area (pig-latinize (.getText text-field)))
        (.setCaretPosition text-area (.getLength (.getDocument text-area))))))
  (doto panel
    (.setFocusable true) ; won't generate key events without this
    (.addKeyListener panel)
    (.setLayout (BoxLayout. panel 3)) ;3 refers to BoxLayout.PAGE_AXIS
    (.add text-field)
    (.add text-area)
    )  
  (doto frame
    (.add panel)
    (.pack)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)))

(defn main []
  (let [frame (JFrame. "ig-Pay atinizer-Lay")
        width 50
        height 20
        cell-size 10
        ;key-code-atom (atom nil)
        panel-width (* width cell-size)
        panel-height (* height cell-size)
        panel (create-panel panel-width panel-height)
        text-field (create-text-field panel-width)
        text-area (create-text-area panel-width (- panel-height (.getHeight text-field)))]
    (configure-gui frame panel text-field text-area)))

;(main)

(def perms ["usc", "ucs",
           "suc", "scu",
           "cus", "csu"])

(def dictionary
  (->> (slurp "/usr/share/dict/words")
    s/split-lines
	  (map s/lower-case)
	  (into #{})))

(defn suck[]
  (filter #(.contains % 
             (doseq [i (range 0 5)]
               (perms i))
             dictionary)))
