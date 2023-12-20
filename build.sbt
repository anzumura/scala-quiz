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
// a lot of tests use temporary files which can cause problems when run in parallel
Test / parallelExecution := false

lazy val root = (project in file("."))
  .settings(
    name := "scala-quiz",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    assembly / mainClass := Some("quiz.main.Main")
  )
