import AssemblyKeys._
import scala.sys.process.Process

name := "Coursera"

version := "0.0.2"

scalaVersion := "2.10.2"

crossScalaVersions := Seq("2.9.2", "2.10.2")

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.9" withSources() excludeAll(
    ExclusionRule(organization = "junit")
)

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.1"

libraryDependencies += "org.streum" %% "configrity-core" % "1.0.0"

libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.11"

org.scalastyle.sbt.ScalastylePlugin.Settings

assemblySettings

jarName in assembly := "coursera.jar"

test in assembly := {}

target in assembly := file("target/out")

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {_.data.getName == "scalatest_2.10-1.9.1.jar"}
}

TaskKey[Unit]("deploy") := {
    val assemblyFile = "target/out/coursera.jar"
    val linuxFile = "coursera"
    val templateFile = "src/main/resources/template"
    val command = "(cat %1$s; cat %2$s) > %3$s; chmod +x %3$s" format (templateFile, assemblyFile, linuxFile)
    Process(Seq("sh", "-c", command)).!
    val homeBinDir = System.getProperty("user.home") + "/bin"
    val fhomeBinDir = new File(homeBinDir)
    if (fhomeBinDir.exists() && fhomeBinDir.isDirectory()) {
        val command = "(cat %1$s; cat %2$s) > %3$s; chmod +x %3$s" format (templateFile, assemblyFile, "%s/%s".format(homeBinDir, linuxFile))
        Process(Seq("sh", "-c", command)).!
    }
}

TaskKey[Unit]("deploy") <<= TaskKey[Unit]("deploy").dependsOn(assembly)
