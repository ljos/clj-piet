(ns clj-piet.editor
  (:import javax.swing.JFrame)
  (:import java.awt.Color)
  (:require [clj-piet.interpreter :as interpreter :only (colours)]))

(def colours (map (partial map #(eval `(Color. ~@%)))
                  (partition 6 (keys interpreter/colours))))

(defn -main []
  (let [frame (doto (JFrame. "Piet Editor")
                (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                (.setSize 800 600)
                (.setVisible true))
        graphics (doto (.getGraphics frame)
                   (.setColor Color/WHITE)
                   (.fillRect 0 0 800 600)
                   (.setColor Color/BLACK))]
    (doseq [x (doall (map (partial * 20) (range 1 (int (/ 800 20)))))]
    (.drawLine graphics x 40 x 580))
    (doseq [y (map (partial * 20) (range 1 (/ 600 20)))]
      (.drawLine graphics 20 y (- 800 20) y))
    (doseq [n (range (count colours))
            :let [line (nth colours n)]]
      (doseq [m (range (count line))
              :let [colour (nth line m)]]
        (doto graphics
          (.setColor colour)
          (.fillRect (+ (* n 20) 720) (* (+ 3 m) 20) 20 20))))))