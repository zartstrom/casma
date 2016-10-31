name := "Casma"

scalaVersion := "2.11.8"

version := "0.1"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.11.0-M4",
  "com.github.finagle" %% "finch-circe" % "0.11.0-M4",
  "io.circe" %% "circe-generic" % "0.5.3",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalameta" %% "scalameta" % "1.2.0",
  "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1")
//TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")



