(defproject clj "1.0.0"
  :repositories {"releases" "repo.maven.org/maven2"}
  :dev-dependencies [[lein-javac "1.2.1-SNAPSHOT"]
                     [clojurecheck "2.0.2"]]
  :main clj.BasicWorker
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.cli "0.1.0"]
                 [com.googlecode.efficient-java-matrix-library/ejml "0.17"]
                 [congomongo "0.1.5-SNAPSHOT"]]
  :target-dir "target/")
