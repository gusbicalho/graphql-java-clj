(ns graphql-java-clj.executor.compile.union
  (:require [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLTypeReference GraphQLUnionType)))

(s/defn compile-union :- GraphQLUnionType
  [interface-name :- String
   {:keys [description, members]} :- gjw.exec.schemas/UnionDefinition]
  (let [builder (-> (GraphQLUnionType/newUnionType)
                    (.name interface-name)
                    (cond-> description (.description ^String description)))]
    (doseq [member-key members
            :let [member-name (name member-key)]]
      (.possibleType builder (GraphQLTypeReference/typeRef member-name)))
    (.build builder)))
