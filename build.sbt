name := "Casma"

scalaVersion := "2.12.0"

version := "0.1"

libraryDependencies ++= Seq(
  // no scala 2.12 yet
  //"com.github.finagle" %% "finch-core" % "0.11.0-M4",
  //"com.github.finagle" %% "finch-circe" % "0.11.0-M4",
  //"org.scalameta" %% "scalameta" % "1.3.0",
  "io.circe" %% "circe-generic" % "0.6.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1")
//TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")



