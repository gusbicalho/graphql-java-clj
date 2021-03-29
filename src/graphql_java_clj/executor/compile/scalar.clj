(ns graphql-java-clj.executor.compile.scalar
  (:require [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [schema.core :as s])
  (:import (graphql.language BooleanValue FloatValue IntValue StringValue Value)
           (graphql.schema Coercing CoercingParseLiteralException CoercingParseValueException CoercingSerializeException GraphQLScalarType)))

(defn- default-parse-literal [parse-value]
  (fn parse-literal [^Value input & _]
    (cond
      (instance? StringValue input)
      (parse-value (.getValue ^StringValue input))

      (instance? IntValue input)
      (parse-value (.getValue ^IntValue input))

      (instance? FloatValue input)
      (parse-value (.getValue ^FloatValue input))

      (instance? BooleanValue input)
      (parse-value (.isValue ^BooleanValue input)))))

(defn- coercing [{:keys [serialize, parse-value, parse-literal]}]
  (let [parse-literal (or parse-literal (default-parse-literal parse-value))]
    (reify
      Coercing
      (serialize [_ value]
        (try
          (serialize value)
          (catch Exception ex
            (throw (CoercingSerializeException. ex)))))

      (parseValue [_ input]
        (try
          (parse-value input)
          (catch Exception ex
            (throw (CoercingParseValueException. ex)))))

      (parseLiteral [_ input]
        (try
          (parse-literal input)
          (catch Exception ex
            (throw (CoercingParseLiteralException. ex)))))

      (parseLiteral [_ input variables]
        (try
          (parse-literal input variables)
          (catch Exception ex
            (throw (CoercingParseLiteralException. ex))))))))

(s/defn compile-scalar :- GraphQLScalarType
  [scalar-name :- String
   {:keys [description] :as scalar-def} :- gjw.exec.schemas/ScalarDefinition]
  (-> (GraphQLScalarType/newScalar)
      (.name scalar-name)
      (cond-> description (.description ^String description))
      (.coercing (coercing scalar-def))
      (.build)))
