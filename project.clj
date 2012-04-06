(defproject clj-piet "1.0.0-SNAPSHOT"
  :description "Interpreter for piet written in cljoure"
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :jvm-opts ["-Djava.awt.headless=true"]
  :main clj-piet.core)