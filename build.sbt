
name := "SecondScalaWeek"

version := "0.1"

scalaVersion := "2.12.7"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.mockito" % "mockito-core" % "2.7.22" % Test
libraryDependencies += "org.pegdown" % "pegdown" % "1.6.0" % Test

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/test-reports")


libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.2.1",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",
  "mysql" % "mysql-connector-java" % "6.0.6"
)
