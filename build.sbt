
ThisBuild / organization := "com.github.itoooo"
ThisBuild / scalaVersion := "2.12.3"
ThisBuild / version := "0.4.0"

enablePlugins(SbtOsgi)

lazy val root = (project in file(".")).
  settings(
    name := "scala-graphql",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.26",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",

    osgiSettings,

    OsgiKeys.bundleSymbolicName := "scala-graphql",
    OsgiKeys.exportPackage := Seq("com.github.itoooo.graphql.*"),
    OsgiKeys.privatePackage := Seq("com.github.itoooo.graphql.ast.*"),

    publishTo := Some(Resolver.mavenLocal)
  )
