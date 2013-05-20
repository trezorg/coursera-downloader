import AssemblyKeys._

name := "Coursera"

version := "0.0.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.7"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.1"

libraryDependencies += "org.streum" %% "configrity-core" % "1.0.0"

libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.11"

org.scalastyle.sbt.ScalastylePlugin.Settings

assemblySettings

jarName in assembly := "coursera.jar"

test in assembly := {}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {_.data.getName == "scalatest_2.10-1.9.1.jar"}
}

