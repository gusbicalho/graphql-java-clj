(ns graphql-java-clj.executor.compile.enum
  (:require [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLEnumType GraphQLEnumValueDefinition)))

(s/defn ^:private ->GraphQLEnumValueDefinition :- GraphQLEnumValueDefinition
  [{:keys [name value description deprecated]} :- gjw.exec.schemas/EnumValueDefinition]
  (-> (GraphQLEnumValueDefinition/newEnumValueDefinition)
      (.name ^String name)
      (.value value)
      (cond-> description (.description ^String description))
      (cond-> deprecated (.deprecationReason ^String deprecated))
      (.build)))

(s/defn compile-enum :- GraphQLEnumType
  [enum-name :- String
   {:keys [description, values]} :- gjw.exec.schemas/EnumDefinition]
  (let [builder (-> (GraphQLEnumType/newEnum)
                    (.name enum-name)
                    (cond-> description (.description ^String description)))]
    (doseq [value values]
      (.value builder (->GraphQLEnumValueDefinition value)))
    (.build builder)))
