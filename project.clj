(defproject tappan/tappan "0.0.1-SNAPSHOT"
  :description "The core of Tappan, also its growing warm ember."
  :warn-on-reflection true
  :repositories {"releases" "repo.maven.org/maven2"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.cli "0.1.0"]
                 [die-roboter "1.0.0-SNAPSHOT"]
                 [tappan/matrix "0.0.1-SNAPSHOT"]
                 [tappan/q "0.0.1-SNAPSHOT"]
                 [tappan/util "0.0.1-SNAPSHOT"]
                 [cljfastdtw "1.0.4-SNAPSHOT"]
                 [congomongo "0.1.7"]]
  :target-dir "target/"
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :clj-stacktrace {:test-color true})
