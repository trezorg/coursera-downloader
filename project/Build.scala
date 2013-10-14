import java.io._
import sbt._
import sbtassembly.Plugin.AssemblyKeys._


object IOFile {

  val buffer = 1024 * 4

  def concatFiles(outFile: String, inFiles: Seq[String]): File = {
    val out: OutputStream  =
      new BufferedOutputStream(new FileOutputStream(outFile))
    val buf = new Array[Byte](buffer)
    try {
      inFiles.foreach { file =>
        val in = new BufferedInputStream(new FileInputStream(file))
        var count = 0
        while ({ count = in.read(buf, 0, buffer); count != -1}) {
          out.write(buf, 0, count)
          out.flush()
        }
        in.close()
      }
    } finally {
      out.close()
    }
    val newOut = new File(outFile)
    if (newOut.exists()) newOut.setExecutable(true, false)
    newOut
  }

}

object Build extends Build {

  import IOFile._

  val deploy = TaskKey[Unit]("deploy", "Make executable Linux file")

  val copyToBin = settingKey[Boolean]("Either copy the binary file to user's home bin directory.")
  val linuxFile = settingKey[String]("Name of the binary Linux file.")

  val deployTask = deploy <<= (copyToBin in deploy, linuxFile in deploy, outputPath in assembly) map {
      (toBin: Boolean, linuxFile: String, assemblyFile: File) =>
    val templateFile = "src/main/resources/template"
    val out: File =
      concatFiles(linuxFile, List(templateFile, assemblyFile.getCanonicalPath))
    println("Created file %s".format(out.getCanonicalPath))
    if (toBin) {
      val homeBinDir = System.getProperty("user.home") + "/bin"
      val fhomeBinDir = file(homeBinDir)
      if (fhomeBinDir.exists && fhomeBinDir.isDirectory) {
        val out = concatFiles(
          new File(fhomeBinDir, linuxFile).getCanonicalPath,
          List(templateFile, assemblyFile.getCanonicalPath))
        println("Created file %s".format(out.getCanonicalPath))
      }
    }
  }

  lazy val root = Project(
    "Coursera",
    file("."),
    settings = Project.defaultSettings ++ Seq(
      deployTask,
      deploy <<= deploy.dependsOn(assembly),
      copyToBin := false,
      linuxFile := "coursera"
    )
  )

}

