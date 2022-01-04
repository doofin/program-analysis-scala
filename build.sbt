ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "ucScala",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.12.8",
      "com.lihaoyi" %% "pprint" % "0.5.3",
      "com.github.julien-truffaut" % "monocle-core_2.12" % "2.1.0"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
