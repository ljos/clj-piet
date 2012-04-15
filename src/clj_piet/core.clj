(ns clj-piet.core
  (:use [clj-piet.interpreter :only (piet-interpreter)])
  (:gen-class))

(defn -main [image codel-size]
  (piet-interpreter image (read-string codel-size)))