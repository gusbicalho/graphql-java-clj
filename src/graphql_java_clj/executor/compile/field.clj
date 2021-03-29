(ns graphql-java-clj.executor.compile.field
  (:require [graphql-java-clj.executor.compile.type :as gjw.exec.compile-type]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLArgument GraphQLFieldDefinition)))

(s/defn compile-argument :- GraphQLArgument
  [argument-name :- String
   {:keys [type, description, default-value]} :- gjw.exec.schemas/ArgumentDefinition]
  (-> (GraphQLArgument/newArgument)
      (.name argument-name)
      (cond-> description (.description ^String description))
      (cond-> default-value (.defaultValue default-value))
      (.type (gjw.exec.compile-type/compile-input-type-ref type))
      (.build)))

(s/defn compile-field :- GraphQLFieldDefinition
  [field-name :- String
   {:keys [type, description, args, deprecated]} :- gjw.exec.schemas/FieldDefinition]
  (let [builder (-> (GraphQLFieldDefinition/newFieldDefinition)
                    (.name field-name)
                    (cond-> description (.description ^String description))
                    (cond-> deprecated (.deprecate ^String deprecated))
                    (.type (gjw.exec.compile-type/compile-output-type-ref type)))]
    (doseq [[arg-key arg-def] args]
      (.argument builder (compile-argument (name arg-key) arg-def)))
    (.build builder)))
