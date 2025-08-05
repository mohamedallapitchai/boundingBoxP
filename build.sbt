import sbt.project
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

ThisBuild / scalaVersion := "3.3.5"
lazy val root = (project in file("."))
  .settings(
    mainClass := Some("BoundingBox"),
    name := "BoundingBox",
  )


