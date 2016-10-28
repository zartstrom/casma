name := "DataFinch"

scalaVersion := "2.11.8"

version := "0.1"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.11.0-M4",
  "com.github.finagle" %% "finch-circe" % "0.11.0-M4",
  "io.circe" %% "circe-generic" % "0.5.3"
  //"org.coursera" %% "autoschema" % "0.1"
)

