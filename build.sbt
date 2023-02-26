ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "result-scala"
  )

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.15" % Test,
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)
