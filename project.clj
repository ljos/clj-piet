(defproject clj-piet "0.0.1"
  :description "Interpreter for piet written in cljoure"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :jvm-opts ["-Djava.awt.headless=true"]
  :main clj-piet.core)