package by.trezor.coursera

import java.io.File
import java.util.logging._
import scala.collection.JavaConverters.asScalaIteratorConverter
import scalaj.http._
import org.rogach.scallop._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.streum.configrity._


object CouseraParser {

  private final val RootElement = ".course-item-list-section-list"
  private final val HeaderSubElement = "h3"
  private final val ChapterElement = ".course-lecture-item-resource"

  def parsePage(st: String): List[List[String]] = {
    (for (el: Element <- Jsoup.parse(st).select(RootElement).iterator().asScala) yield parseElement(el)).toList
  }

  private def parseElement(el: Element): List[String] = {
    val header: String = el.previousElementSibling().select(HeaderSubElement).text().toLowerCase.
      replaceAll("""(?i).*Week\s*(\d+):\s*""", "$1.").replaceAll("""[\s,]+""", "-").replaceAll("""\p{javaSpaceChar}""", "")
    header :: (
      for (
        links: Element <- el.select(ChapterElement).iterator().asScala;
        link: Element <- links.select("a").iterator().asScala
      ) yield Http.urlDecode(link.attr("href"))
    ).toList
  }

}

class CourseraOptions(arguments: Seq[String]) extends ScallopConf(arguments) {

  banner("""Usage: coursera [OPTIONS] [classname]
           |This program helps to download coursera class lectures
           |Options:
           |""".stripMargin)
  val username = opt[String](required = false, descr = "username/email")
  val password = opt[String](required = false, descr = "password")
  val filename = opt[String](required = false, descr = "configuration file")
  val info = opt[Boolean](required = false, descr = "show files to download", default = Some(false))
  val directory = opt[String](required = false,
    descr = "directory to save files. current directory by default.",
    validate = (a => new File(a).isDirectory))
  val chapter = opt[List[Int]](required = false,
    descr = "lecture chapters to download",
    validate = (a => a.forall(_ > 0)))
  val classname= trailArg[String](required = false, descr = "Coursera class name")
  mutuallyExclusive(username, filename)
  mutuallyExclusive(password, filename)
  codependent(username, password)

}


class Coursera(course: String, username: String, password: String, directory: String, info: Boolean) {

  private final val SessionIdCookieName = "sessionid"
  private final val SessionCookieName = "session"
  private final val MaestroLoginCookieName = "maestro_login"
  private final val MaestroLoginFlagName = "maestro_login_flag"
  private final val CsrfTokenCookieName = "csrf_token"
  private final val PasswordFieldName = "password"
  private final val UsernameFieldName = "email_address"
  private final val ConnectionTimeout = 20000
  private final val LectureUrl = "http://class.coursera.org/%s/lecture/index"
  private final val loginUrl = "https://www.coursera.org/maestro/api/user/login"
  private final val RefererUrl = "https://www.coursera.org"
  private final val ClassAuthUrl = "https://class.coursera.org/%s/auth/auth_redirector?" +
    "type=login&subtype=normal&email=&visiting=%s"

  private final val LOG = Logger.getLogger("coursera")

  lazy val getLectureUrl: String = String.format(LectureUrl, course)

  val couseraHttpOptions: List[HttpOptions.HttpOption] =
    List(HttpOptions.connTimeout(ConnectionTimeout), HttpOptions.readTimeout(ConnectionTimeout))

  lazy val getClassAuthUrl: String = String.format(ClassAuthUrl, course, Http.urlEncode(getLectureUrl))

  val cookiesNames = List(SessionIdCookieName, MaestroLoginCookieName, MaestroLoginFlagName)

  def cookiesToMap(st: String): Map[String, String] = {
    st.split("; ").map(_.split("=", 2)).foldLeft(Map[String, String]())((m, s) => m + (
      s match {
        case Array(a, b) => a -> b
        case _ => "" -> ""
      }
      )
    )
  }

  lazy val getCsrfToken: String = {
    Http(getLectureUrl).options(couseraHttpOptions).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie") match {
      case Some(list: List[String]) => cookiesToMap(list.head) getOrElse(CsrfTokenCookieName, "")
      case _ => ""
    }
  }

  def prepareLoginHeaders(csrf: String): List[(String,String)] =
    List(("Referer", RefererUrl), ("X-CSRFToken", csrf), ("Cookie", "csrftoken=" + csrf))

  def prepareLoginParams: List[(String, String)] =
    List((UsernameFieldName, username), (PasswordFieldName, password))

  def prepareClassAuthHeaders: List[(String, String)] = {
    val classAuthCookie = for (
      map <- getAuthCookie.map(cookiesToMap(_));
      (name, value) <- map
      if (cookiesNames.contains(name)))
    yield name + "=" + value
    List(("Cookie", classAuthCookie mkString "; "))
  }

  def prepareSessionHeaders(session: String): List[(String, String)] =
    List(("Cookie", SessionCookieName + "=" + session))

  lazy val getAuthCookie: List[String] = {
    Http.post(loginUrl).headers(prepareLoginHeaders(getCsrfToken)).
      options(couseraHttpOptions).
      params(prepareLoginParams).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie").head
  }

  def getSessionCookie: String = {
    Http(getClassAuthUrl).headers(prepareClassAuthHeaders).
      options(couseraHttpOptions).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie") match {
      case Some(list: List[String]) => {
        for (
          map <- list.map(cookiesToMap(_));
          (name, value) <- map
          if (name == SessionCookieName)
        ) yield value}.mkString
      case _ => ""
    }
  }

  def getClassPage: String = {
    Http(getLectureUrl).headers(prepareSessionHeaders(getSessionCookie)).
      options(couseraHttpOptions).asString
  }

  def getFilesList(chapters: Option[List[Int]]): List[(List[String], Int)] = {
    val files = CouseraParser.parsePage(getClassPage).zipWithIndex
    chapters match {
      case Some(list) => files.filter(x => list.contains(x._2 + 1))
      case None => files
    }
  }

  def downloadFile(directory: String, url: String) {

  }

  def downloadChapters(files: List[List[String]]) {
      files.foreach(chapter => processChapter(chapter))
  }

  def processChapter(chapter: List[String]) {
    val dir = new File(directory, chapter.head)
    if (!dir.exists && !dir.isDirectory && !dir.mkdirs) {
      LOG.log(Level.SEVERE, "Cannot create directory " + dir.getCanonicalPath)
    } else {
      chapter.foreach(url => downloadFile(dir.getCanonicalPath, url))
    }
  }

  def showChapter(chapter: List[String], index: Int) {
    println("%slecture #%s: %s%s" format(Console.RED, index, chapter.head, Console.RESET))
    println()
    chapter.tail.zipWithIndex.foreach(x => println("%s%s: %s%s" format(Console.GREEN, x._2, x._1, Console.RESET)))
    println()
  }

  def showChapters(files: List[(List[String], Int)]) {
    files.foreach(x => showChapter(x._1, x._2 + 1))
  }

  def getFilePath(filename: String): String = new File(directory, filename).getCanonicalPath

  def apply(chapters: Option[List[Int]]) {
    val files = getFilesList(chapters)
    if (info) {
      showChapters(files)
    } else {
      downloadChapters(files.map(_._1))
    }
  }

}

object CourseraConfig {

  val defaultConfigFile = "coursera.conf"

  def getConfigFilenamePath(filename: Option[String]): Option[String] = {
    val file = filename match {
      case Some(result) => result
      case None         => defaultConfigFile
    }
    if (new File(file).exists) {
      Some(file)
    } else {
      // relative path
      val absFilename = new File(new File(".").getAbsolutePath, file)
      if (absFilename.exists && absFilename.isFile) {
        Some(absFilename.getCanonicalPath)
      } else {
        val absFilename = new File(new File(System.getProperty("user.home")).getAbsolutePath, file)
        if (absFilename.exists && absFilename.isFile) {
          Some(absFilename.getCanonicalPath)
        } else {
          None
        }
      }
    }
  }

  def readConfig(filename: Option[String]): (Option[String], Option[String], Option[String]) = {
    getConfigFilenamePath(filename) match {
      case Some(file) => parseConfig(file)
      case None => throw new IllegalArgumentException("Cannot find a configuration file")
    }
  }

  def parseConfig(filename: String): (Option[String], Option[String], Option[String]) = {
    val config = Configuration.load(filename)
    (config.get[String]("username"), config.get[String]("password"), config.get[String]("classname"))
  }

}


object Main {

  private final val LOG = Logger.getLogger("coursera")

  def main(args: Array[String]) {
    
    val conf = new CourseraOptions(args)

    def getClassName = if (conf.classname.isEmpty) None else Some(conf.classname())
    def getFileName = if (conf.filename.isEmpty) None else Some(conf.filename())
    def getChapters = if (conf.chapter.isEmpty) None else Some(conf.chapter())
    def getDirectory = if (conf.directory.isEmpty) new File(".").getCanonicalPath else conf.directory()
    def getInfo = conf.info()

    def getCredentials(conf: CourseraOptions): (Option[String], Option[String], Option[String]) = {
      if (conf.username.isEmpty || conf.password.isEmpty) {
        // we should read conf file
        CourseraConfig.readConfig(getFileName)
      } else {
        (Some(conf.username()), Some(conf.password()), getClassName)
      }
    }

    getCredentials(conf) match {
      case (Some(username), Some(password), Some(classname)) => {
        val coursera = new Coursera(classname, username, password, getDirectory, getInfo)(getChapters)
        println(coursera)
      }
      case (Some(username), Some(password), None) => {
        getClassName match {
          case Some(classname) => {
            val coursera = new Coursera(classname, username, password, getDirectory, getInfo)(getChapters)
            println(coursera)
          }
          case None => {
            LOG.info("Should be set classname parameter either in a configuration file or in a command line")
            conf.printHelp()
            sys.exit(1)
          }
        }
      }
      case _ => {
        LOG.info("Should be set either a configuration filename or an username and password")
        conf.printHelp()
        sys.exit(1)
      }
    }
  }
}
