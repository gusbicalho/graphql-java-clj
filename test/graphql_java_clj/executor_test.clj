(ns graphql-java-clj.executor-test
  (:require [graphql-java-clj.executor :as gjw.exec]
            [graphql-java-clj.executor.schemas :as gjw.exec.schemas]
            [graphql-java-clj.executor.utils :as gjw.exec.utils]
            [matcher-combinators.midje :refer [match]]
            [midje.sweet :refer :all]
            [schema.core :as s])
  (:import (graphql GraphqlErrorBuilder)
           (graphql.execution DataFetcherResult)
           (graphql.schema DataFetchingEnvironment)
           (java.util UUID)))

(defn str->uuid [s]
  (UUID/fromString s))

(s/def schema-definition :- gjw.exec.schemas/SchemaDefinition
  {:mutation
   :MutationType,

   :query
   :QueryType,

   :types
   [{:name       :UUID,
     :kind       :scalar,
     :definition {:description "Valid UUID",
                  :serialize   str
                  :parse-value str->uuid}}
    {:name       :PersonType,
     :kind       :enum,
     :definition {:description "Person or company",
                  :values      [{:name "PERSON", :value :person}
                                {:name "COMPANY", :value :company}]}}
    {:name       :SavingsAccountStatus,
     :kind       :enum,
     :definition {:values [{:name "ACTIVE", :value :active}
                           {:name "CANCELED", :value :canceled}
                           {:name "BLOCKED", :value :blocked, :deprecated "We don't do that anymore"}]}}
    {:name       :MutationType,
     :kind       :object,
     :definition {:fields
                  {:cancelAccount {:type :CancelAccountResult,
                                   :args {:input {:type '(non-null :CancelAccountInput)}}}}}}
    {:name       :QueryType,
     :kind       :object,
     :definition {:fields
                  {:viewer           {:type :Customer},
                   :customer         {:type :Customer
                                      :args {:customerId {:type '(non-null :UUID)}}},
                   :people           {:type '(non-null (list (non-null :Someone)))},
                   :broken           {:type 'Int},
                   :resolvedCustomly {:type     'Int
                                      :resolver :query/notFollowingTheConvention}}}}
    {:name       :Customer,
     :kind       :object,
     :definition {:fields
                  {:id               {:type '(non-null :UUID)},
                   :name             {:type '(non-null String)},
                   :personIdentifier {:type 'String},
                   :personType       {:type       :PersonType
                                      :deprecated "Nope"},
                   :savingsAccount   {:type :SavingsAccount},
                   :widgets          {:type '(non-null (list (non-null :Widget)))}}}}
    {:name       :SavingsAccount,
     :kind       :object,
     :definition {:fields
                  {:id     {:type '(non-null :UUID)}
                   :status {:type '(non-null :SavingsAccountStatus)}}}}
    {:name       :CoolWidget,
     :kind       :object,
     :definition {:implements [:Titled :Widget],
                  :fields     {:title   {:type '(non-null String)},
                               :message {:type 'String},
                               :howCool {:type '(non-null Int)}}}}
    {:name       :BoringWidget,
     :kind       :object,
     :definition {:implements [:Titled :Widget],
                  :fields     {:title     {:type '(non-null String)},
                               :message   {:type 'String},
                               :howBoring {:type '(non-null Int)}}}}
    {:name       :Contact,
     :kind       :object,
     :definition {:fields
                  {:name        {:type '(non-null String)}
                   :phoneNumber {:type 'String}}}}
    {:name       :CancelAccountResult,
     :kind       :object,
     :definition {:fields
                  {:failure        {:type 'String}
                   :savingsAccount {:type :SavingsAccount}}}}
    {:name       :CancelAccountInput,
     :kind       :input-object,
     :definition {:fields
                  {:savingsAccountId {:type '(non-null :UUID)}
                   :reason           {:type '(non-null String)}}}}
    {:name       :Titled
     :kind       :interface
     :definition {:fields
                  {:title {:type '(non-null String)}}}}
    {:name       :Widget,
     :kind       :interface,
     :definition {:implements [:Titled]
                  :fields     {:title   {:type '(non-null String)}
                               :message {:type 'String}}}}
    {:name       :Someone
     :kind       :union
     :definition {:members [:Customer :Contact]}}]})

(def context
  {:customers                       {#uuid "0-0-0-0-42" {:id               #uuid "0-0-0-0-42"
                                                         :name             "Frodo"
                                                         :personIdentifier "12345678910"
                                                         :personType       :person}
                                     #uuid "0-0-0-0-37" {:id         #uuid "0-0-0-0-37"
                                                         :name       "Fellowship of The Ring"
                                                         :personType :company}
                                     #uuid "1-0-0-0-0"  {:id #uuid "1-0-0-0-0"}}
   :customer-id->savings-account-id {#uuid "0-0-0-0-42" #uuid "0-0-0-42-1"
                                     #uuid "0-0-0-0-37" #uuid "0-0-0-37-1"}
   :savings-accounts                {#uuid "0-0-0-42-1" {:id     #uuid "0-0-0-42-1"
                                                         :status :active}
                                     #uuid "0-0-0-37-1" {:id     #uuid "0-0-0-37-1"
                                                         :status :canceled}}})

(s/def resolvers :- gjw.exec.schemas/ResolversDefinition
  {:Customer/savingsAccount
   (fn customer->savings-account [{:keys [context source]}]
     (let [customer-id        (:id source)
           savings-account-id (get-in context [:customer-id->savings-account-id customer-id])]
       (get-in context [:savings-accounts savings-account-id])))

   :Customer/widgets
   (fn customer->widgets [_]
     [^{:graphql/type :CoolWidget} {:title   "Cool"
                                    :message "This event is cool"
                                    :howCool 9001}
      ^{:graphql/type :BoringWidget} {:title     "Boring"
                                      :message   "There are boring events, too"
                                      :howBoring -5}])

   :CancelAccountResult/savingsAccount
   (fn cancel-account->savings-account [{:keys [context, source]}]
     (let [savings-account-id (:savingsAccountId source)]
       (get-in context [:savings-accounts savings-account-id])))

   :QueryType/viewer
   (fn [{:keys [context]}]
     (get-in context [:customers #uuid "0-0-0-0-42"]))

   :QueryType/customer
   (fn [{:keys [context arguments]}]
     (get-in context [:customers (:customerId arguments)]))

   :QueryType/people
   (fn people [{:keys [^DataFetchingEnvironment environment context]}]
     (gjw.exec.utils/supply-async
       (fn []
         (-> (DataFetcherResult/newResult)
             (.data [(get-in context [:customers #uuid "0-0-0-0-42"])
                     {:name        "Will"
                      :phoneNumber "555-5555"}])
             (.error (-> (GraphqlErrorBuilder/newError environment)
                         (.message
                           "Exception while fetching data (%s) : Unknown error"
                           (into-array Object [(-> environment (.getExecutionStepInfo) (.getPath))]))
                         (.extensions {"reason" :some-people-not-found})
                         (.build)))
             (.build)))))

   :QueryType/broken
   (fn [{:keys [environment]}]
     (-> (GraphqlErrorBuilder/newError environment)
         (.message
           "Exception while fetching data (%s) : Broken!!!"
           (into-array Object [(-> ^DataFetchingEnvironment environment (.getExecutionStepInfo) (.getPath))]))
         (.extensions {"reason" :not-unbroken})
         (.toResult)))

   :MutationType/cancelAccount
   (fn cancel-account [{:keys [context arguments]}]
     (let [{savings-account-id :savingsAccountId
            reason             :reason} (:input arguments)
           savings-account (get-in context [:savings-accounts savings-account-id])]
       (if (not= "ACTIVE" (:status savings-account))
         {:failure          "Account not active"
          :savingsAccountId savings-account-id}
         {:failure          (str "Not an acceptable reason: " reason)
          :savingsAccountId savings-account-id})))

   :query/notFollowingTheConvention
   (fn not-following-the-convention [_]
     42)})

(s/def type-resolvers :- gjw.exec.schemas/TypeResolversDefinition
  {:Someone (fn [v] (if (:id v) :Customer :Contact))})

(defn run-query!
  ([query, variables]
   (run-query!
     query, variables
     (gjw.exec/new-graphql
       (gjw.exec/compile-schema schema-definition resolvers type-resolvers {}))))
  ([query, variables, graphql]
   (gjw.exec/run-query! graphql context query nil variables)))

(facts "queries at the default executor"

  (fact "simple query"
    (run-query!
      "query Query { viewer { id name personIdentifier } }"
      {})
    => {:data {:viewer {:id               "00000000-0000-0000-0000-000000000042",
                        :name             "Frodo",
                        :personIdentifier "12345678910"}}})

  (fact "query with variables"
    (run-query!
      "query Query($cusId : UUID!) { customer(customerId: $cusId) { id name personIdentifier } }"
      {:cusId "0-0-0-0-37"})
    => {:data {:customer {:id               "00000000-0000-0000-0000-000000000037",
                          :name             "Fellowship of The Ring",
                          :personIdentifier nil}}})

  (fact "query with variables and an error"
    (run-query!
      "query Query($cusId : UUID!) { customer(customerId: $cusId) { id name personIdentifier } }"
      {:cusId "1-0-0-0-0"})
    => {:errors [{:message
                              "The field at path '/customer/name' was declared as a non null type, but the code involved in retrieving data has wrongly returned a null value.  The graphql specification requires that the parent field be set to null, or if that is non nullable that it bubble up null to its parent and so on. The non-nullable type is 'String' within parent type 'Customer'",
                  :path       ["customer" "name"],
                  :extensions {:classification "NullValueInNonNullableField"}}],
        :data   {:customer nil}})

  (fact "query engaging nested resolvers"
    (run-query!
      "query Query { viewer { id name personType savingsAccount { id status } } }"
      {})
    => {:data {:viewer {:id             "00000000-0000-0000-0000-000000000042",
                        :name           "Frodo",
                        :personType     "PERSON",
                        :savingsAccount {:id "00000000-0000-0000-0042-000000000001", :status "ACTIVE"}}}})

  (fact "mutation"
    (run-query!
      "mutation Mutation($input : CancelAccountInput!) { cancelAccount(input: $input) { failure savingsAccount { id status } } }"
      {:input {:savingsAccountId "0-0-0-37-1", :reason "some reason"}})
    => {:data {:cancelAccount {:failure "Account not active",
                               :savingsAccount
                                        {:id     "00000000-0000-0000-0037-000000000001",
                                         :status "CANCELED"}}}})

  (fact "mutation with different arguments"
    (run-query!
      "mutation Mutation($input : CancelAccountInput!) { cancelAccount(input: $input) { failure savingsAccount { id status } } }"
      {:input {:savingsAccountId "0-0-0-42-1", :reason "some reason"}})
    => {:data {:cancelAccount {:failure        "Account not active",
                               :savingsAccount {:id     "00000000-0000-0000-0042-000000000001"
                                                :status "ACTIVE"}}}})

  (fact "query with interfaces"
    (run-query!
      "query Query { viewer { widgets { title message ... on CoolWidget { howCool } ... on BoringWidget { howBoring } } } }"
      {})
    => {:data {:viewer {:widgets [{:title "Cool", :message "This event is cool", :howCool 9001}
                                  {:title "Boring", :message "There are boring events, too", :howBoring -5}]}}})

  (fact "query with unions"
    (run-query!
      "query Query { people { __typename ... on Customer { name personType } ... on Contact { name phoneNumber } } }"
      {})
    => {:errors [{:message    "Exception while fetching data (/people) : Unknown error",
                  :locations  [{:line 1, :column 14}],
                  :path       ["people"],
                  :extensions {:reason :some-people-not-found, :classification "DataFetchingException"}}],
        :data   {:people [{:__typename "Customer", :name "Frodo", :personType "PERSON"}
                          {:__typename "Contact", :name "Will", :phoneNumber "555-5555"}]}})

  (fact "query with a custom error"
    (run-query!
      "query Query { broken }"
      {})
    => {:errors [{:message    "Exception while fetching data (/broken) : Broken!!!",
                  :locations  [{:line 1, :column 14}],
                  :path       ["broken"],
                  :extensions {:reason :not-unbroken, :classification "DataFetchingException"}}],
        :data   {:broken nil}})

  (fact "query exercising a field with a resolver specified via :resolver key"
    (run-query!
      "query Query { resolvedCustomly }"
      {})
    => {:data {:resolvedCustomly 42}}))
