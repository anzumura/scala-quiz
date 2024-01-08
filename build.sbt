ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.0-RC1"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-explain", "-explain-types", "-feature",
  "-print-lines", "-unchecked", "-Xfatal-warnings", "-Xmigration")

lazy val libs = project.settings(
  name := "libs",
  // a lot of tests use temp files which can cause problems when run in parallel
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.10.0", "org.scalatest" %% "scalatest" % "3.2.17" % Test))

lazy val quiz = project.dependsOn(libs)
  .settings(name := "quiz", assembly / mainClass := Some("quiz.Main"))
