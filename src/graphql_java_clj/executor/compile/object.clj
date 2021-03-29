(ns graphql-java-clj.executor.compile.object
  (:require [graphql-java-clj.executor.compile.field :as gjw.exec.compile-field]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLObjectType GraphQLTypeReference)))

(s/defn compile-object :- GraphQLObjectType
  [object-name :- String
   {:keys [description, fields, implements]} :- gjw.exec.schemas/ObjectDefinition]
  (let [builder (-> (GraphQLObjectType/newObject)
                    (.name object-name)
                    (cond-> description (.description ^String description)))]
    (doseq [interface-key implements
            :let [interface-name (name interface-key)]]
      (.withInterface builder (GraphQLTypeReference/typeRef interface-name)))
    (doseq [[field-key field] fields]
      (.field builder (gjw.exec.compile-field/compile-field (name field-key) field)))
    (.build builder)))
