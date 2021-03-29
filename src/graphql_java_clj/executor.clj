(ns graphql-java-clj.executor
  (:require [clojure.walk :as walk]
            [graphql-java-clj.executor.compile.code-registry :as gjw.exec.compile-code-registry]
            [graphql-java-clj.executor.compile.enum :as gjw.exec.compile-enum]
            [graphql-java-clj.executor.compile.input-object :as gjw.exec.compile-input-object]
            [graphql-java-clj.executor.compile.interface :as gjw.exec.compile-interface]
            [graphql-java-clj.executor.compile.object :as gjw.exec.compile-object]
            [graphql-java-clj.executor.compile.scalar :as gjw.exec.compile-scalar]
            [graphql-java-clj.executor.compile.union :as gjw.exec.compile-union]
            [graphql-java-clj.executor.defaults :as gjw.exec.defaults]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [graphql-java-clj.executor.utils :as gjw.exec.utils]
            [schema.core :as s])
  (:import (graphql ExecutionInput ExecutionResult GraphQL)
           (graphql.schema GraphQLObjectType GraphQLSchema GraphQLType)
           (java.util.function BiConsumer Function)))

(s/defn compile-type :- GraphQLType
  [type-definition :- (s/cond-pre GraphQLType gjw.exec.schemas/AnyTypeDefinition)]
  (if (instance? GraphQLType type-definition)
    type-definition
    (let [{:keys    [kind definition]
           type-key :name} type-definition
          type-name (name type-key)]
      (case kind
        :scalar (gjw.exec.compile-scalar/compile-scalar type-name definition)
        :enum (gjw.exec.compile-enum/compile-enum type-name definition)
        :object (gjw.exec.compile-object/compile-object type-name definition)
        :input-object (gjw.exec.compile-input-object/compile-input-object type-name definition)
        :interface (gjw.exec.compile-interface/compile-interface type-name definition)
        :union (gjw.exec.compile-union/compile-union type-name definition)))))

(s/defn ^:private find-operation-type :- GraphQLObjectType
  [type-key :- s/Keyword
   types :- #{GraphQLType}]
  (let [type-name (name type-key)]
    (->> types
         (filter
          (s/fn [type]
            (and (instance? GraphQLObjectType type)
                 (= type-name (.getName ^GraphQLObjectType type)))))
         (first))))

(s/defn compile-schema :- GraphQLSchema
  ([schema-definition :- gjw.exec.schemas/SchemaDefinition
    resolvers :- gjw.exec.schemas/ResolversDefinition
    type-resolvers :- gjw.exec.schemas/TypeResolversDefinition]
   (compile-schema schema-definition resolvers type-resolvers {}))
  ([schema-definition :- gjw.exec.schemas/SchemaDefinition
    resolvers :- gjw.exec.schemas/ResolversDefinition
    type-resolvers :- gjw.exec.schemas/TypeResolversDefinition
    options :- gjw.exec.schemas/Options]
   (s/validate gjw.exec.schemas/SchemaDefinition schema-definition)
   (let [options       (gjw.exec.defaults/merge-default-options options)
         types         (set (map compile-type (:types schema-definition)))
         code-registry (gjw.exec.compile-code-registry/compile-code-registry schema-definition resolvers type-resolvers options)
         query         (-> (:query schema-definition)
                           (find-operation-type types))
         mutation      (some-> (:mutation schema-definition)
                               (find-operation-type types))]
     (-> (GraphQLSchema/newSchema)
         (.additionalTypes types)
         (.query query)
         (cond-> mutation (.mutation mutation))
         (.codeRegistry code-registry)
         (.build)))))

(s/defn new-graphql :- GraphQL
  [schema :- GraphQLSchema]
  (-> (GraphQL/newGraphQL schema)
      (.build)))

(s/defn ->ExecutionInput :- ExecutionInput
  [context :- s/Any
   query :- s/Str
   operation-name :- (s/maybe s/Str)
   variables :- {s/Keyword s/Any}]
  (-> (ExecutionInput/newExecutionInput)
      (.query query)
      (cond-> operation-name (.operationName operation-name))
      (.variables (walk/stringify-keys variables))
      (.context context)
      (.build)))

(defn to-clj-specification [^ExecutionResult result]
  (->> ^ExecutionResult result
       (.toSpecification)
       (gjw.exec.utils/java->clj)))

(s/defn run-query-async!
  [graphql :- GraphQL
   context :- s/Any
   query :- s/Str
   operation-name :- (s/maybe s/Str)
   variables :- {s/Keyword s/Any}]
  (let [promise (promise)]
    (-> (.executeAsync
          graphql
          (->ExecutionInput context query operation-name variables))
        (.thenApply
          (reify
            Function
            (apply [_ result]
              (to-clj-specification result))))
        (.whenComplete
          (reify
            BiConsumer
            (accept [_ result exception]
              (deliver promise [result exception])))))
    (delay
      (let [[result exception] @promise]
        (if exception
          (throw exception)
          result)))))

(s/defn run-query!
  [graphql :- GraphQL
   context :- s/Any
   query :- s/Str
   operation-name :- (s/maybe s/Str)
   variables :- {s/Keyword s/Any}]
  (-> (->ExecutionInput context query operation-name variables)
      (->> (.execute graphql))
      (to-clj-specification)))
