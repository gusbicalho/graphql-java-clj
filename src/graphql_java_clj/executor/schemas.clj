(ns graphql-java-clj.executor.schemas
  (:require [schema.core :as s])
  (:import (clojure.lang Var)
           (graphql.language Value)
           (graphql.schema DataFetcher DataFetcherFactory GraphQLType TypeResolver)))

(s/defschema TypeModifier
  (s/enum 'non-null 'list))

(s/defschema TypeSpec
  (s/cond-pre
    s/Symbol
    s/Keyword
    s/Str
    [(s/one TypeModifier 'modifier)
     (s/recursive #'TypeSpec)]))

(s/defschema ScalarSerializeFn
  (s/=> s/Any s/Any))

(s/defschema ScalarParseValueFn
  (s/=> s/Any s/Any))

(s/defschema ScalarParseLiteralFn
  (s/=>* s/Any
         [Value]
         [Value {String s/Any}]))

(s/defschema ScalarDefinition
  {(s/optional-key :description)   s/Str
   :serialize                      ScalarSerializeFn
   :parse-value                    ScalarParseValueFn
   (s/optional-key :parse-literal) ScalarParseLiteralFn})

(s/defschema EnumValueDefinition
  {:name                         s/Str
   :value                        s/Any
   (s/optional-key :description) s/Str
   (s/optional-key :deprecated)  s/Str})

(s/defschema EnumDefinition
  {(s/optional-key :description) s/Str
   :values                       [EnumValueDefinition]})

(s/defschema ArgumentDefinition
  {(s/optional-key :description)   s/Str
   :type                           TypeSpec
   (s/optional-key :default-value) s/Any})

(s/defschema FieldDefinition
  {(s/optional-key :description) s/Str
   :type                         TypeSpec
   (s/optional-key :args)        {s/Keyword ArgumentDefinition}
   (s/optional-key :resolver)    s/Keyword
   (s/optional-key :deprecated)  s/Str})

(s/defschema ObjectDefinition
  {(s/optional-key :description) s/Str
   (s/optional-key :implements)  [s/Keyword]
   :fields                       {s/Keyword FieldDefinition}})

(s/defschema InputFieldDefinition
  {(s/optional-key :description) s/Str
   :type                         TypeSpec})

(s/defschema InputObjectDefinition
  {(s/optional-key :description) s/Str
   :fields                       {s/Keyword InputFieldDefinition}})

(s/defschema InterfaceDefinition
  {(s/optional-key :description)   s/Str
   (s/optional-key :implements)    [s/Keyword]
   (s/optional-key :type-resolver) s/Keyword
   :fields                         {s/Keyword FieldDefinition}})

(s/defschema UnionDefinition
  {(s/optional-key :description)   s/Str
   (s/optional-key :type-resolver) s/Keyword
   :members                        [s/Keyword]})

(s/defschema AnyTypeDefinition
  (letfn [(kind-is [kind]
            (fn [v]
              (= kind (:kind v))))
          (typedef [kind schema]
            {:name       s/Keyword
             :kind       (s/eq kind)
             :definition schema})]
    (s/conditional
      (kind-is :scalar) (typedef :scalar ScalarDefinition)
      (kind-is :enum) (typedef :enum EnumDefinition)
      (kind-is :object) (typedef :object ObjectDefinition)
      (kind-is :interface) (typedef :interface InterfaceDefinition)
      (kind-is :union) (typedef :union UnionDefinition)
      (kind-is :input-object) (typedef :input-object InputObjectDefinition))))

(s/defschema SchemaTypeDefinitions
  [(s/cond-pre GraphQLType AnyTypeDefinition)])

(s/defschema SchemaDefinition
  {:query                     s/Keyword
   (s/optional-key :mutation) s/Keyword
   :types                     SchemaTypeDefinitions})

(s/defschema ResolverFn
  (s/=> s/Any & [s/Any]))

(s/defschema ResolversDefinition
  {s/Keyword (s/cond-pre Var ResolverFn)})

(s/defschema TypeResolverFn
  (s/=> s/Any & [s/Any]))

(s/defschema TypeResolversDefinition
  {s/Keyword (s/cond-pre Var TypeResolverFn)})

(s/defschema ResolverAsDataFetcherFn
  (s/=> DataFetcher
        ResolverFn))

(s/defschema TypeResolverWrapperFn
  (s/=> TypeResolver
        TypeResolverFn))

(s/defschema Options
  {(s/optional-key :resolver->DataFetcher)       ResolverAsDataFetcherFn
   (s/optional-key :default-DataFetcherFactory)  DataFetcherFactory
   (s/optional-key :type-resolver->TypeResolver) TypeResolverWrapperFn
   (s/optional-key :default-TypeResolver)        TypeResolver})
