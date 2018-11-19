ThisBuild / organization := "com.github.itoooo"
ThisBuild / scalaVersion := "2.12.3"
ThisBuild / version := "0.3.0-SNAPSHOT"

lazy val root = (project in file(".")).
  settings(
    name := "scala-graphql",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "com.typesafe.akka" % "akka-http_2.12" % "10.0.9",
    libraryDependencies += "io.spray" % "spray-json_2.12" % "1.3.4",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",
    publishTo := Some(Resolver.file("file", new File("repository")))
  )
