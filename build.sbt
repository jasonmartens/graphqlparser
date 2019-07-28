name := "graphqlparser"

version := "0.1"

scalaVersion := "2.12.8"

//libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4"
//val circeVersion = "0.12.0-M4"
//libraryDependencies ++= Seq(
//  "io.circe" %% "circe-core",
//  "io.circe" %% "circe-generic",
//  "io.circe" %% "circe-parser"
//).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.28",
//  "io.argonaut" %% "argonaut" % "6.2.3",
//  "io.argonaut" %% "argonaut-scalaz" % "6.2.3",
  "org.json4s" %% "json4s-core" % "3.6.7",
  "org.json4s" %% "json4s-native" % "3.6.7",
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

scalacOptions ++= Seq(
  "-Xlog-implicits",
  "-Ypartial-unification")
