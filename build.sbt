name := "src/main/slang"
version := "1.0.0"
scalaVersion := "2.12.8"
scalacOptions := Seq(
  "-target:jvm-1.8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-encoding",
  "utf8",
  "-Ywarn-macros:after",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:privates",
  "-language:higherKinds"
)
val scalaTestVersion = "3.1.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)
scalafmtVersion := "1.5.1"

scalafmtConfig := file(".scalafmt.conf")

scalafmtOnCompile := true
