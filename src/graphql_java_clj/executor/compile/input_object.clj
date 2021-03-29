(ns graphql-java-clj.executor.compile.input-object
  (:require [graphql-java-clj.executor.compile.type :as gjw.exec.compile-type]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema GraphQLInputObjectField GraphQLInputObjectType)))

(s/defn compile-field :- GraphQLInputObjectField
  [field-name :- String
   {:keys [description, type]} :- gjw.exec.schemas/InputFieldDefinition]
  (-> (GraphQLInputObjectField/newInputObjectField)
      (.name field-name)
      (cond-> description (.description ^String description))
      (.type (gjw.exec.compile-type/compile-input-type-ref type))
      (.build)))

(s/defn compile-input-object :- GraphQLInputObjectType
  [object-name :- String
   {:keys [description, fields]} :- gjw.exec.schemas/InputObjectDefinition]
  (let [builder (-> (GraphQLInputObjectType/newInputObject)
                    (.name object-name)
                    (cond-> description (.description ^String description)))]
    (doseq [[field-key field] fields]
      (.field builder (compile-field (name field-key) field)))
    (.build builder)))
