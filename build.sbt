ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explain",
  "-explain-types",
  "-feature",
  "-print-lines",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xmigration"
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-quiz",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )
