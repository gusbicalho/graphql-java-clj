(ns graphql-java-clj.executor.utils
  (:require [clojure.walk :as walk]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (java.util List Map)
           (java.util.concurrent CompletableFuture)
           (java.util.function Supplier)))

(s/defn java->clj [java-collection]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    (walk/prewalk
      (fn [x]
        (cond
          (or (map? x)
              (and (seqable? x)
                   (instance? Map x)))
          (into {} (map f x))

          (and (not (vector? x))
               (instance? List x))
          (vec x)

          :else
          x))
      java-collection)))

(s/defn enum-values :- [gjw.exec.schemas/EnumValueDefinition]
  ([value->name :- (s/=> s/Str s/Any)
    values :- [s/Any]]
   (enum-values value->name identity values))
  ([value->name :- (s/=> s/Str s/Any)
    value->value :- (s/=> s/Any s/Any)
    values :- [s/Any]]
   (map (fn [value]
          (if (map? value)
            value
            {:name  (value->name value)
             :value (value->value value)}))
        values)))

(defn supply-async [f]
  ; TODO better name
  (CompletableFuture/supplyAsync
    (reify
      Supplier
      (get [_]
        (f)))))
