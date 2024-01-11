ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.0-RC1"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-explain", "-explain-types", "-feature",
  "-print-lines", "-unchecked", "-Xfatal-warnings", "-Xmigration")
ThisBuild / wartremoverErrors ++= Warts.allBut(
  Wart.Any, Wart.DefaultArguments, Wart.Equals, Wart.ImplicitConversion, Wart.MutableDataStructures,
  Wart.Overloading, Wart.SeqApply, Wart.StringPlusAny, Wart.Throw, Wart.Var, Wart.While)

lazy val libs = project.settings(
  // some tests use temp files which can cause problems when run in parallel
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.10.0", "org.scalatest" %% "scalatest" % "3.2.17" % Test))

lazy val quiz = project.dependsOn(libs).settings(assembly / mainClass := Some("quiz.Main"))
