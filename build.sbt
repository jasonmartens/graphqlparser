name := "graphqlparser"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0-M1",
  "com.lihaoyi" %% "upickle" % "0.7.5"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions ++= Seq(
  "-Xlog-implicits",
  "-Ypartial-unification")
