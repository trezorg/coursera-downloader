name := "Coursera"

version := "0.0.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.7"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"
