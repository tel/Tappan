(defproject tappan/tappan "0.0.1"
  :description "The core of Tappan, also its growing warm ember."
  :warn-on-reflection true
  :repositories {"releases" "repo.maven.org/maven2"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [tappan/q "0.0.1-SNAPSHOT"]
                 [tappan/matrix "0.0.1-SNAPSHOT"]
                 [org.clojars.pallix/analemma "1.0.0-SNAPSHOT"]
                 [cljfastdtw "1.0.4-SNAPSHOT"]
                 [org.clojars.sunng/beanstalk "1.0.6"]
                 [congomongo "0.1.7"]]
  :target-dir "target/")
