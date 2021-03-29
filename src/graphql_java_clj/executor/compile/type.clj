(ns graphql-java-clj.executor.compile.type
  (:require [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql Scalars)
           (graphql.schema GraphQLInputType GraphQLList GraphQLNonNull GraphQLOutputType GraphQLTypeReference)))

(def ^:private known-types
  {"String"  Scalars/GraphQLString
   "Float"   Scalars/GraphQLFloat
   "Int"     Scalars/GraphQLInt
   "Boolean" Scalars/GraphQLBoolean
   "ID"      Scalars/GraphQLID})

(defn- compile-type-ref [type-spec]
  (cond
    (or (symbol? type-spec)
        (keyword? type-spec)
        (string? type-spec))
    (let [type-name (name type-spec)]
      (or (get known-types type-name)
          (GraphQLTypeReference/typeRef type-name)))

    (list? type-spec)
    (let [[modifier modified-type-spec] type-spec]
      (case modifier
        non-null (GraphQLNonNull/nonNull
                   (compile-type-ref modified-type-spec))
        list (GraphQLList/list
               (compile-type-ref modified-type-spec))
        nil))))

(s/defn compile-output-type-ref :- GraphQLOutputType
  [type-spec :- gjw.exec.schemas/TypeSpec]
  (or (compile-type-ref type-spec)
      (throw (ex-info "Cannot compile output type" {:type type-spec}))))

(s/defn compile-input-type-ref :- GraphQLInputType
  [type-spec :- gjw.exec.schemas/TypeSpec]
  (or (compile-type-ref type-spec)
      (throw (ex-info "Cannot compile input type" {:type type-spec}))))
