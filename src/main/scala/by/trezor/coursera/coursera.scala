package by.trezor.coursera

import java.io.{FileOutputStream, BufferedInputStream, BufferedOutputStream, File}
import java.util.logging._
import java.net.{HttpURLConnection, URL, URLDecoder}
import scala.collection.JavaConverters.asScalaIteratorConverter
import scalaj.http._
import scala.concurrent.{future, Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.reflectiveCalls
import scala.actors.Actor
import scala.actors.Actor._
import org.rogach.scallop._
import org.fusesource.jansi.AnsiConsole
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.streum.configrity._
import org.fusesource.jansi.Ansi._


object CourseraOutput {

  def out(op: String => Unit)(text: String, color: Option[Color] = None): CourseraOutput.type = {
    color match {
      case Some(cl) => {
        AnsiConsole.systemInstall()
        op(ansi().fg(cl).a(text).reset().toString)
      }
      case _ => op(text)
    }
    this
  }
  def outPrintln = out(println) _
  def outPrint = out(print) _
  def prnln(text: String, color: Option[Color] = None): CourseraOutput.type = outPrintln(text, color)
  def prn(text: String, color: Option[Color] = None): CourseraOutput.type = outPrint(text, color)
  def apply(text: String, color: Option[Color] = None) = prnln(text, color)

}

object CourseraParser {

  private final val RootElement = ".course-item-list-section-list"
  private final val HeaderSubElement = "h3"
  private final val ChapterElement = ".course-lecture-item-resource"

  def parsePage(st: String): List[List[String]] = {
    CourseraOutput("Getting files list...")
    (for (el: Element <- Jsoup.parse(st).select(RootElement).iterator().asScala) yield parseElement(el)).toList
  }

  private def parseElement(el: Element): List[String] = {
    val header: String = el.previousElementSibling().select(HeaderSubElement).text().trim().replaceAll("\u00a0","")
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
  val username = opt[String](required = false, descr = "set username/email")
  val password = opt[String](required = false, descr = "set password")
  val filename = opt[String](required = false, descr = "set configuration file")
  val info = opt[Boolean](required = false, descr = "show available files and exit", default = Some(false))
  val force = opt[Boolean](required = false, descr = "force re-download files if exist", default = Some(false), short = 'u')
  val pdf = opt[Boolean](required = false, descr = "get only pdf files", default = Some(false), short = 'm')
  val txt = opt[Boolean](required = false, descr = "get only txt files", default = Some(false))
  val srt = opt[Boolean](required = false, descr = "get only srt files", default = Some(false))
  val pptx = opt[Boolean](required = false, descr = "get only pptx files", default = Some(false))
  val mp4 = opt[Boolean](required = false, descr = "get only mp4 files", default = Some(false))
  val directory = opt[String](required = false,
    descr = "set directory to save files. default is a current directory.",
    validate = a => new File(a).isDirectory)
  val chapter = opt[List[Int]](required = false,
    descr = "set lecture chapters by number to download. -c 1 2 3",
    validate = a => a.forall(_ > 0))
  val classname= trailArg[String](required = false, descr = "set Coursera class name")
  mutuallyExclusive(username, filename)
  mutuallyExclusive(password, filename)
  codependent(username, password)
}


class Coursera(course: String, username: String, password: String, directory: String, optionsMap: Map[String, Boolean]) {

  val SessionIdCookieName           = "sessionid"
  val SessionCookieName             = "session"
  val MaestroLoginCookieName        = "maestro_login"
  val MaestroLoginFlagName          = "maestro_login_flag"
  val CsrfTokenCookieName           = "csrf_token"
  val PasswordFieldName             = "password"
  val UsernameFieldName             = "email_address"
  val ConnectionTimeout             = 3600 * 1000
  val OkResponseCode                = 200
  val RedirectResponseCode          = 302
  val LectureUrl                    = "http://class.coursera.org/%s/lecture/index"
  val loginUrl                      = "https://www.coursera.org/maestro/api/user/login"
  val ReferrerUrl                   = "https://www.coursera.org"
  val ClassAuthUrl                  = "https://class.coursera.org/%s/auth/auth_redirector?type=login&subtype=normal&email=&visiting=%s"
  val DefaultCodePage               = "UTF-8"
  val txt                           = optionsMap.getOrElse("txt", false)
  val srt                           = optionsMap.getOrElse("srt", false)
  val pdf                           = optionsMap.getOrElse("pdf", false)
  val mp4                           = optionsMap.getOrElse("mp4", false)
  val pptx                          = optionsMap.getOrElse("pptx", false)
  val update                        = optionsMap.getOrElse("update", false)
  val LOG = Logger.getLogger("coursera")
  def isTxt(filename: String)       = txt && filename.endsWith(".txt")
  def isPdf(filename: String)       = pdf && filename.endsWith(".pdf")
  def isSrt(filename: String)       = srt && filename.endsWith(".srt")
  def isMp4(filename: String)       = mp4 && filename.endsWith(".mp4")
  def isPptx(filename: String)      = pptx && filename.endsWith(".pptx")

  lazy val getLectureUrl: String = String.format(LectureUrl, course)

  val courseraHttpOptions: List[HttpOptions.HttpOption] =
    List(HttpOptions.connTimeout(ConnectionTimeout), HttpOptions.readTimeout(ConnectionTimeout))

  lazy val getClassAuthUrl: String = String.format(ClassAuthUrl, course, Http.urlEncode(getLectureUrl))

  val cookiesNames = List(SessionIdCookieName, MaestroLoginCookieName, MaestroLoginFlagName)

  def cookiesToMap(st: String): Map[String, String] = {
    st.split("; ").map(_.split("=", 2)).foldLeft(Map[String, String]())((m, s) => m + (
      s match {
        case Array(a, b) => a -> b
        case _ => "" -> ""
      }))
  }

  lazy val getCsrfToken: String = {
    Http(getLectureUrl).options(courseraHttpOptions).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie") match {
      case Some(list: List[String]) => cookiesToMap(list.head) getOrElse(CsrfTokenCookieName, "")
      case _ => ""
    }
  }

  def prepareLoginHeaders(csrf: String): List[(String,String)] =
    List(("Referer", ReferrerUrl), ("X-CSRFToken", csrf), ("Cookie", "csrftoken=" + csrf))

  def prepareLoginParams: List[(String, String)] =
    List((UsernameFieldName, username), (PasswordFieldName, password))

  def prepareClassAuthHeaders: List[(String, String)] = {
    val classAuthCookie = for (
      map <- getAuthCookie.map(cookiesToMap(_));
      (name, value) <- map
      if cookiesNames.contains(name))
    yield name + "=" + value
    List(("Cookie", classAuthCookie mkString "; "))
  }

  lazy val sessionHeaders: List[(String, String)] =
    List(("Cookie", SessionCookieName + "=" + getSessionCookie))

  lazy val getAuthCookie: List[String] = {
    Http.post(loginUrl).headers(prepareLoginHeaders(getCsrfToken)).
      options(courseraHttpOptions).
      params(prepareLoginParams).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie").head
  }

  def getSessionCookie: String = {
    CourseraOutput("Authenticating...")
    Http(getClassAuthUrl).headers(prepareClassAuthHeaders).
      options(courseraHttpOptions).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie") match {
      case Some(list: List[String]) => {
        for (
          map <- list.map(cookiesToMap(_));
          (name, value) <- map
          if name == SessionCookieName
        ) yield value}.mkString
      case _ => ""
    }
  }

  def getClassPage: String = Http(getLectureUrl).headers(sessionHeaders).options(courseraHttpOptions).asString

  def getFilesList(chapters: Option[List[Int]]): List[(List[String], Int)] = {
    val files = CourseraParser.parsePage(getClassPage).zipWithIndex
    chapters match {
      case Some(list) => files.filter(x => list.contains(x._2 + 1))
      case None => files
    }
  }

  def needDownloadFile(file: File, size: Long): Boolean = {
    update || !file.exists() || size != -1 && file.length != size
  }

  def downloadFile(file: File, url: String, session: String, size: Long): (Boolean, String) = {
    if (needDownloadFile(file, size)) {
      new Downloader(file.getCanonicalPath, url, session)()
      (true, file.getName)
    } else {
      (false, file.getName)
    }
  }

  def downloadChapters(files: Iterable[(String, List[(Int, String, String)])]) {
    CourseraOutput("Starting download files...")
    files.foreach { case(name, list) => downloadChapter(name, list) }
  }

  def downloadChapter(chapterDirectory: String, chapter: List[(Int, String, String)]) {
    val dir = new File(directory, chapterDirectory)
    if (!dir.exists && !dir.isDirectory && !dir.mkdirs) {
      LOG.log(Level.SEVERE, "Cannot create directory " + dir.getCanonicalPath)
    } else {
      val session = sessionHeaders(0)._2
      val tasks: List[Future[(Boolean, String)]] = chapter.map {
        case(size, name, url) => future { downloadFile(new File(dir, name), url, session, size) }
      }
      tasks.foreach(f = f => {
        f onComplete {
          case Success((true, filename)) => terminalWaitActor ! Message(filename, state = true)
          case Success((false, filename)) => terminalWaitActor ! Message(filename, state = false)
          case Failure(t) => CourseraOutput("An error has occurred: " + t.getMessage)
        }
      })
      val futures: Future[List[(Boolean, String)]] = Future.sequence(tasks)
      Await.result(futures, 120.minutes)
    }
  }

  def showChapter(index: Int, chapterDirectory: String, chapter: List[(Int, String, String)]) {
    CourseraOutput.prn("#%s: " format index + 1, Some(Color.BLUE))(chapterDirectory, Some(Color.RED))
    println()
    val dir = new File(directory, chapterDirectory).getCanonicalPath
    chapter.zipWithIndex.foreach {
      case ((size, name, url), i) => {
        val file = new File(dir, name)
        if (!file.exists()) {
          CourseraOutput("%s: %s" format(i, name), Some(Color.GREEN))
        } else if (size != -1 && file.length != size) {
          CourseraOutput("%s: %s" format(i, name), Some(Color.BLUE))
        } else {
          CourseraOutput("%s: %s" format(i, name), Some(Color.CYAN))
        }
      }
    }
    println()
  }

  def showChapters(files: Map[Int, (String, List[(Int, String, String)])]) {
    files.toIndexedSeq.sortBy(_._1).foreach {case (index, (dir, list)) => showChapter(index, dir, list) }
  }

  def parseContentDisposition(value: String): String = value.split("=").toList.last.replaceAll("\"*$|^\"*", "")

  def parseUrlFilename(url: String): String = url.split("/").toList.last

  def getContentDisposition(headers: Map[String, List[String]]): Option[String] = headers.get("Content-Disposition") match {
    case Some(List(value)) => Option(parseContentDisposition(value))
    case _ => None
  }

  def parseUriParameters(uri: String): Map[String, List[String]] = {
    val params = collection.mutable.Map[String, List[String]]()
    val parts = uri split "\\?"
    if (parts.length > 1) {
      val query = parts(1)
      query.split("&") map { param =>
        val pair = param split "="
        val key = URLDecoder.decode(pair(0), DefaultCodePage)
        val value = pair.length match {
          case l if l > 1 => URLDecoder.decode(pair(1), DefaultCodePage)
          case _ => ""
        }
        val values = params get key match {
          case Some(list: List[String]) => list :+ value
          case None => List(value)
        }
        params += (key -> values)
        values
      }
    }
    params.toMap
  }

  def getFileData(url: String): Option[(Int, String, String)] = {
    // returns content-length, filename and url
    val response = Http(url).headers(sessionHeaders).
      options(courseraHttpOptions).option(HttpOptions.method("HEAD")).asCodeHeaders
    response match {
      case (OkResponseCode, headers) => {
        Some((
          headers.getOrElse("Content-Length", List("-1"))(0).toInt,
          getContentDisposition(headers) match {
            case Some(value) => value
            case _ => parseUrlFilename(url)
          },
          url
        ))
      }
      case (RedirectResponseCode, headers) => {
        val location = headers.get("Location").get(0)
        val res = Http(location).options(courseraHttpOptions).asCodeHeaders
        Some((
          res._2.getOrElse("Content-Length", List("-1"))(0).toInt,
          getContentDisposition(headers) match {
            case Some(value) => value
            case _ => parseContentDisposition(parseUriParameters(location).get("response-content-disposition").get(0))
          },
          location
        ))
      }
      case (code, headers) => {
        LOG.log(Level.SEVERE, "Cannot get file data for url: %s . Response code: %s" format(url, code))
        None
      }
    }
  }

  def getLectureData(files: List[String], title: String): List[(Int, String, String)] = {
    CourseraOutput.prn("[%s] - " format title, Some(Color.RED))("Getting files data...")
    val tasks: List[Future[Option[(Int, String, String)]]] = files.map { url => future { getFileData(url) }}
    val futures: Future[List[Option[(Int, String, String)]]] = Future.sequence(tasks)
    Await.result(futures, 60.minutes).toList.flatten
  }

  val notSetAny =  List(txt, pdf, mp4, srt, pptx).forall(x => !x)

  def filterFile(filename: String): Boolean = {
    notSetAny || isTxt(filename) || isMp4(filename) || isPdf(filename) || isSrt(filename) || isPptx(filename)
  }

  def getLecturesData(files: List[(List[String], Int)]): Map[Int, (String, List[(Int, String, String)])] = {
    files.map {
      case ((title :: urls), number) => (number, (title, getLectureData(urls, title).filter(x => filterFile(x._2))))
      case (_, number) => (number, ("", List()))
    }.toMap
  }

  def apply(chapters: Option[List[Int]]) {
    terminalWaitActor.start()
    try {
      val files = getLecturesData(getFilesList(chapters))
      val filesCount = files.values.foldLeft(0)((b, a) => b + a._2.length)
      terminalWaitActor ! Count(filesCount)
      if (optionsMap.getOrElse("info", false)) {
        showChapters(files)
      } else {
        downloadChapters(files.values)
      }
    } catch {
      case e: Exception => CourseraOutput("An error has occurred: %s" format e.getMessage)
    } finally {
      terminalWaitActor ! Stop
    }
  }

}

case class Count(count: Int)
case class Message(filename: String, state: Boolean)
case object Stop
case object Progress

object terminalWaitActor extends Actor {

  var count = 0
  var pos = 0
  var filesDone = 0
  val positions = Array("-", "\\", "|", "/")
  val timeToSleep = 200

  def showProgress() {
    pos = (pos + 1)  % 4
    val message =
      if (count > 0) {
        val message = "[%s] done: [%s/%s]".format(positions(pos), filesDone, count)
        "%s%s" format(message, "\b" * message.length)
      } else "[%s]%s".format(positions(pos), "\b" * 3)
    CourseraOutput.prn(message)
    actor {
      Thread.sleep(timeToSleep)
      terminalWaitActor ! Progress
    }
  }

  def act() {
    showProgress()
    loop {
      react {
        case Progress =>
          showProgress()
        case Count(cnt: Int) =>
          count = cnt
        case Message(filename: String, true) =>
          CourseraOutput.prn("Finished: ", Some(Color.GREEN))(filename)
          filesDone += 1
        case Message(filename: String, false) =>
          CourseraOutput.prn("Skipped: ", Some(Color.RED))(filename)
          filesDone += 1
        case Stop =>
          CourseraOutput.prn("Done.", Some(Color.RED))
          exit()
      }
    }
  }

}

object CourseraConfig {

  val defaultConfigFile = "coursera.conf"

  def checkFileExists(file: File): Boolean = file.exists && file.isFile

  def getConfigs(filename: Option[String]): List[String] = {
    val list = List(
      new File(new File(".").getAbsolutePath, defaultConfigFile),
      new File(new File(System.getProperty("user.home")).getAbsolutePath, defaultConfigFile))
    (if (filename.isDefined) new File(filename.get) :: list else list).filter(checkFileExists).map(_.getCanonicalPath)
  }

  def readConfigs(filename: Option[String]): (Option[String], Option[String], Option[String]) = {
    val data = mergeConfigs(getConfigs(filename) map parseConfig)
    (data(0), data(1), data(2))
  }

  def mergeConfigs(list: List[(Option[String], Option[String], Option[String])]): List[Option[String]] = {
    val (a, b, c) = list.unzip3
    List(a, b, c).map(_.collect { case Some(x) => x }).map(x => if (x.isEmpty) None else Some(x(0)))
  }

  def parseConfig(filename: String): (Option[String], Option[String], Option[String]) = {
    val config = Configuration.load(filename)
    (config.get[String]("username"), config.get[String]("password"), config.get[String]("classname"))
  }

}

class Downloader(path: String, location: String, session: String) {

  private final val Buffer = 1024 * 5
  private final val LOG = Logger.getLogger("coursera")

  def download() {
    var out: Option[BufferedOutputStream] = None
    var in: Option[BufferedInputStream] = None
    try {
      val url = new URL(location)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection.addRequestProperty("Cookie", session)
      in = Some(new BufferedInputStream(connection.getInputStream))
      out = Some(new BufferedOutputStream(new FileOutputStream(path)))
      var count = 0
      val buffer: Array[Byte] = new Array[Byte](Buffer)
      while ({ count = in.get.read(buffer, 0, Buffer); count != -1}) out.get.write(buffer, 0, count)
    } catch {
      case e:Exception => LOG.log(Level.SEVERE, "Cannot download file: " + path, e)
    } finally {
      in match {
        case Some(value) => value.close()
        case _ =>
      }
      out match {
        case Some(value) => value.close()
        case _ =>
      }
    }
  }

  def apply() {
    download()
  }

}

object Main {

  private final val LOG = Logger.getLogger("coursera")

  def main(args: Array[String]) {

    val conf = new CourseraOptions(args)

    val className  = if (conf.classname.isEmpty) None else Some(conf.classname())
    val fileName   = if (conf.filename.isEmpty) None else Some(conf.filename())
    val chapters   = if (conf.chapter.isEmpty) None else Some(conf.chapter())
    val directory  = if (conf.directory.isEmpty) new File(".").getCanonicalPath else conf.directory()
    val optionsMap = Map(
      "info" -> conf.info(),
      "update" -> conf.force(),
      "pdf" -> conf.pdf(),
      "txt" -> conf.txt(),
      "srt" -> conf.srt(),
      "mp4" -> conf.mp4(),
      "pptx" -> conf.pptx()
    )

    def getCredentials(conf: CourseraOptions): (Option[String], Option[String], Option[String]) = {
      if (conf.username.isEmpty || conf.password.isEmpty || className.isEmpty) {
        // we should read conf file
        CourseraConfig.readConfigs(fileName)
      } else {
        (Some(conf.username()), Some(conf.password()), className)
      }
    }

    getCredentials(conf) match {
      case (Some(username), Some(password), Some(classname)) => {
        new Coursera(classname, username, password, directory, optionsMap)(chapters)
      }
      case (Some(username), Some(password), None) => {
        className match {
          case Some(classname) => {
            new Coursera(classname, username, password, directory, optionsMap)(chapters)
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
