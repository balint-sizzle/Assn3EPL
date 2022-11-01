ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "epl"

lazy val assignment3 = (project in file("."))
  .settings(
    name := "Assignment 3",
    libraryDependencies += "com.sksamuel.scrimage" % "scrimage-core" % "4.0.32",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
  )
