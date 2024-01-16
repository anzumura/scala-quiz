ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.2-RC1"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-explain", "-feature", "-print-lines",
  "-unchecked", "-Werror", "-Wunused:all", "-Wvalue-discard")
ThisBuild / wartremoverErrors ++= Warts.allBut(
  Wart.Any, Wart.DefaultArguments, Wart.Equals, Wart.ImplicitConversion, Wart.MutableDataStructures,
  Wart.Overloading, Wart.SeqApply, Wart.StringPlusAny, Wart.Throw, Wart.Var, Wart.While)
ThisBuild / excludeDependencies += "org.scala-lang.modules" % "scala-collection-compat_2.13"

val catsVersion = "2.10.0"
val scalaTestVersion = "3.2.17"
val ammoniteVersion = "3.0.0-M0-60-89836cd8"

lazy val libs = project.settings(
  // some tests use temp files which can cause problems when run in parallel
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "com.lihaoyi" % "ammonite" % ammoniteVersion % Test cross CrossVersion.constant("3.3.1")),
  Test / sourceGenerators += Def.task {
    val file = (Test / sourceManaged).value / "amm.scala"
    IO.write(
      file, "object amm { def main(args: Array[String]): Unit = ammonite.AmmoniteMain.main(args) }")
    Seq(file)
  }.taskValue)

lazy val quiz = project.dependsOn(libs).settings(assembly / mainClass := Some("quiz.Main"))
