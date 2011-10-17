(defproject tappan "0.0.1"
  :repositories {"releases" "repo.maven.org/maven2"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.cli "0.1.0"]
                 [cljfastdtw "1.0.4-SNAPSHOT"]
                 [com.googlecode.efficient-java-matrix-library/ejml "0.17"]
                 [org.clojars.sunng/beanstalk "1.0.6"]
                 [congomongo "0.1.7"]]
  :target-dir "target/")
