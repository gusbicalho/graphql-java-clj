(ns graphql-java-clj.executor.compile.interface
  (:require [graphql-java-clj.executor.compile.field :as gjw.exec.compile-field]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLInterfaceType GraphQLTypeReference)))

(s/defn compile-interface :- GraphQLInterfaceType
  [interface-name :- String
   {:keys [description, implements, fields]} :- gjw.exec.schemas/InterfaceDefinition]
  (let [builder (-> (GraphQLInterfaceType/newInterface)
                    (.name interface-name)
                    (cond-> description (.description ^String description)))]
    (doseq [interface-key implements
            :let [interface-name (name interface-key)]]
      (.withInterface builder (GraphQLTypeReference/typeRef interface-name)))
    (doseq [[field-key field] fields]
      (.field builder (gjw.exec.compile-field/compile-field (name field-key) field)))
    (.build builder)))
