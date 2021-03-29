(ns graphql-java-clj.executor.compile.code-registry
  (:require [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.schema DataFetcher DataFetcherFactory FieldCoordinates GraphQLCodeRegistry GraphQLCodeRegistry$Builder TypeResolver)))

(s/defschema FieldResolverCoordinates
  {:parent                    String
   :field                     String
   (s/optional-key :resolver) s/Keyword})

(s/defn ^:private field-resolver-coordinates :- [FieldResolverCoordinates]
  [schema-types :- gjw.exec.schemas/SchemaTypeDefinitions]
  (letfn [(get-resolver-coordinates [{parent-key       :name
                                      {:keys [fields]} :definition}]
            (for [[field-key field] fields
                  :let [field-name (name field-key)
                        resolver   (:resolver field)]]
              (-> {:parent (name parent-key)
                   :field  field-name}
                  (cond-> resolver
                          (assoc :resolver resolver)))))]
    (->> schema-types
         (filter #(= :object (:kind %)))
         (mapcat get-resolver-coordinates))))

(s/defn ^:private set-default-DataFetcherFactory :- GraphQLCodeRegistry$Builder
  [builder :- GraphQLCodeRegistry$Builder
   data-fetcher-factory :- DataFetcherFactory
   coordinates :- [FieldResolverCoordinates]]
  ; The code below should just work. However, there's a bug:
  ; https://github.com/graphql-java/graphql-java/issues/2145
  (.defaultDataFetcher builder data-fetcher-factory)
  ; So we work around it for now by setting data fetcher in all coordinates
  ; (if need be, they will be overriden later)
  (doseq [{:keys [parent, field]} coordinates]
    (.dataFetcher builder
                  (FieldCoordinates/coordinates ^String parent ^String field)
                  data-fetcher-factory))
  builder)

(def valid-name
  (partial re-matches #"[_A-Za-z][_0-9A-Za-z]*"))

(s/defn populate-data-fetchers!
  [builder :- GraphQLCodeRegistry$Builder
   schema-types :- gjw.exec.schemas/SchemaTypeDefinitions
   resolvers :- gjw.exec.schemas/ResolversDefinition
   resolver->DataFetcher :- gjw.exec.schemas/ResolverAsDataFetcherFn
   default-DataFetcherFactory :- DataFetcherFactory]
  (let [coordinates (field-resolver-coordinates schema-types)]
    (set-default-DataFetcherFactory builder default-DataFetcherFactory coordinates)
    (doseq [[resolver-key resolver-fn] resolvers
            :let [[resolver-ns, resolver-name] ((juxt namespace name) resolver-key)]
            :when resolver-ns
            :when (valid-name resolver-ns)
            :when (valid-name resolver-name)]
      ; Whenever a resolver is named with a key of the form :SomeType/someField
      ; we will install it in the coordinates for SomeType someField automatically
      ; Therefore, we don't need to add :resolver keys to most fields, we only
      ; need to add an appropriately named resolver
      (.dataFetcher builder
                    (FieldCoordinates/coordinates ^String resolver-ns
                                                  ^String resolver-name)
                    ^DataFetcher (resolver->DataFetcher resolver-fn)))
    (doseq [{:keys [parent, field, resolver]} coordinates
            :when resolver
            :let [resolver-fn (get resolvers resolver)]]
      ; However, if we do add a :resolver key to a field, that resolver will be used
      ; It will even override any resolvers that happen to have been named with
      ; a key :parent/field in the resolvers map
      (when-not resolver-fn
        (throw (ex-info (str "Resolver not found: " resolver) {:resolver resolver})))
      (.dataFetcher builder
                    (FieldCoordinates/coordinates ^String parent ^String field)
                    ^DataFetcher (resolver->DataFetcher resolver-fn)))))

(s/defn populate-type-resolvers!
  [builder :- GraphQLCodeRegistry$Builder
   schema-types :- gjw.exec.schemas/SchemaTypeDefinitions
   type-resolvers :- gjw.exec.schemas/TypeResolversDefinition
   type-resolver->TypeResolver :- (s/=> TypeResolver s/Any)
   default-TypeResolver :- (s/maybe TypeResolver)]
  (letfn [(missing-default! [kind interface-name]
            (throw
              (ex-info (str "Falling back to default TypeResolver for " kind " " interface-name ", but none provided")
                       {:interface interface-name})))
          (missing-resolver! [kind, interface-name, resolver-key]
            (throw
              (ex-info (str "Required type-resolver at key " resolver-key " for " kind " " interface-name ", not found")
                       {:interface     interface-name
                        :type-resolver resolver-key})))
          (->TypeResolver [kind, type-name, type-resolver-key]
            (cond
              ; If a union/interface specifies a type-resolver, we will attempt to use it
              type-resolver-key
              (type-resolver->TypeResolver
                (or (get type-resolvers type-resolver-key)
                    (missing-resolver! kind type-name type-resolver-key)))

              ; If none is specified, we will look for a type-resolver with the same name
              ; as the type. If found, we will use that.
              (get type-resolvers (keyword type-name))
              (type-resolver->TypeResolver
                (get type-resolvers (keyword type-name)))

              :else
              (or default-TypeResolver
                  (missing-default! kind type-name))))
          (add-type-resolver! [kind, ^String type-name, type-resolver-key]
            (.typeResolver
              builder type-name
              ^TypeResolver (->TypeResolver kind type-name type-resolver-key)))]
    (doseq [{type-name               :name
             kind                    :kind
             {:keys [type-resolver]} :definition} schema-types
            :when (#{:interface :union} kind)]
      (add-type-resolver! (name kind) (name type-name) type-resolver)))
  builder)

(s/defn compile-code-registry :- GraphQLCodeRegistry
  [{:keys [types]} :- gjw.exec.schemas/SchemaDefinition
   resolvers :- gjw.exec.schemas/ResolversDefinition
   type-resolvers :- gjw.exec.schemas/TypeResolversDefinition
   {:keys [resolver->DataFetcher
           default-DataFetcherFactory
           type-resolver->TypeResolver
           default-TypeResolver]}]
  (let [builder (GraphQLCodeRegistry/newCodeRegistry)]
    (populate-data-fetchers! builder types resolvers
                             resolver->DataFetcher
                             default-DataFetcherFactory)
    (populate-type-resolvers! builder types type-resolvers
                              type-resolver->TypeResolver
                              default-TypeResolver)
    (.build builder)))
