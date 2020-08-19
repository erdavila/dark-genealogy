name := "Dark Genealogy"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
)

libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.7"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
