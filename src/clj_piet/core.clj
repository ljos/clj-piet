(ns clj-piet.core
  (:gen-class)
  (:require [clj-piet.interpreter :only (piet-interpreter) :as interpret]))

(defn -main [image codel-size]
  (interpret/piet-interpreter image (read-string codel-size)))