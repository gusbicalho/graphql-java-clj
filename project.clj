(defproject graphql-java-clj "0.1.0"
  :description "A Clojure wrapper for GraphQL Java"
  :url "https://github.com/gusbicalho/graphql-java-clj"
  :license {:name "The MIT License"
            :url  "http://opensource.org/licenses/MIT"}
  :dependencies [[com.graphql-java/graphql-java "16.1"]
                 [prismatic/schema "1.1.12"]
                 [org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[midje "1.9.9"]
                                  [nubank/matcher-combinators "3.1.4"]
                                  [org.clojure/java.classpath "1.0.0"]
                                  [org.clojure/test.check "1.1.0"]]}}
  :repl-options {:init-ns graphql-java-clj.executor})
