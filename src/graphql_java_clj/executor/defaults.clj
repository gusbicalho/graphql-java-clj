(ns graphql-java-clj.executor.defaults
  (:require [graphql-java-clj.executor.utils :as gjw.exec.utils]
            [schema.core :as s])
  (:import (graphql.schema DataFetcher DataFetcherFactory DataFetchingEnvironment PropertyDataFetcher TypeResolver)
           (java.util.function Function)))

(s/defschema DefaultResolverParameters
  {:environment    DataFetchingEnvironment
   :context        s/Any
   :global-context s/Any
   :local-context  s/Any
   :arguments      s/Any
   :source         s/Any})

(s/defschema DefaultResolverFn
  (s/=> s/Any DefaultResolverParameters))

(s/defschema TypeNameResolverFn
  (s/=> s/Keyword s/Any))

(defn- merge-contexts [context1 context2]
  (cond
    (nil? context2)
    context1

    (and (map? context1) (map? context2))
    (merge context1 context2)

    :else
    context2))

(s/defn resolver->DataFetcher :- DataFetcher
  [resolver :- DefaultResolverFn]
  (reify
    DataFetcher
    (get [_ env]
      (resolver {:environment    env
                 :context        (merge-contexts
                                   (.getContext env)
                                   (.getLocalContext env))
                 :global-context (.getContext env)
                 :local-context  (.getLocalContext env)
                 :arguments      (gjw.exec.utils/java->clj (.getArguments env))
                 :source         (.getSource env)}))))

(s/def default-DataFetcherFactory :- DataFetcherFactory
  (reify
    DataFetcherFactory
    (get [_ env]
      (let [field-name (-> env (.getFieldDefinition) (.getName))
            field-key  (keyword field-name)]
        (PropertyDataFetcher/fetching
          (reify
            ; Normal clj functions do not implement java.util.function.Function
            Function
            (apply [_ source]
              (let [value-by-keyword (get source field-key ::absent)]
                (if (not= ::absent value-by-keyword)
                  value-by-keyword
                  (get source field-name))))))))))

(s/defn type-name-resolver->TypeResolver :- TypeResolver
  [f :- TypeNameResolverFn]
  (reify
    TypeResolver
    (getType [_ env]
      (let [object    (.getObject env)
            type-name (name (f object))]
        (-> env (.getSchema) (.getObjectType type-name))))))

(s/defn type-name-meta-key->TypeResolver :- TypeResolver
  [meta-key]
  (type-name-resolver->TypeResolver
    (fn [value]
      (or (get (meta value) meta-key)
          (throw
            (ex-info
              (str "Expected type name at key " meta-key " of value meta, not found.")
              {:meta-key meta-key
               :meta     (meta value)}))))))

(defn merge-default-options
  [m]
  (merge
    {:resolver->DataFetcher       resolver->DataFetcher
     :default-DataFetcherFactory  default-DataFetcherFactory
     :type-resolver->TypeResolver type-name-resolver->TypeResolver
     :default-TypeResolver        (type-name-meta-key->TypeResolver :graphql/type)}
    m))
