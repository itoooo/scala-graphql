import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.itoooo",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "scala-graphql",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.typesafe.akka" % "akka-http_2.12" % "10.0.9",
    libraryDependencies += "io.spray" % "spray-json_2.12" % "1.3.4",
    libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6",
    publishTo := Some(Resolver.file("file", new File("repository")))
  )
