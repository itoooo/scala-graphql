package com.github.itoooo.graphql

case class Schema(queryType: GraphQlObjectType,
                  mutationType: GraphQlObjectType,
                  subscriptionType: GraphQlObjectType,
                  directiveDefs: Set[GraphqlDirectiveResolver])
