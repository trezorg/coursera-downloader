package by.trezor.coursera

import java.io.{FileOutputStream, BufferedInputStream, BufferedOutputStream, File}
import java.util.logging._
import java.net.{HttpURLConnection, URL, URLDecoder}
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.language.reflectiveCalls
import scala.concurrent.{future, Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.actors.Actor
import scala.actors.Actor._
import scalaj.http._
import org.rogach.scallop._
import org.fusesource.jansi.AnsiConsole
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.streum.configrity._
import org.fusesource.jansi.Ansi._


case class Count(count: Int)
case class Process(size: Long)
case class Message(filename: String, state: Boolean)
case object Stop
case object Progress

class CourseraException(message: String) extends Exception(message)

object CourseraOutput {

  def out(op: String => Unit)(
      text: String,
      color: Option[Color] = None): CourseraOutput.type = {
    color match {
      case Some(cl) =>
        AnsiConsole.systemInstall()
        op(ansi().fg(cl).a(text).reset().toString)
      case _ => op(text)
    }
    this
  }
  def outPrintln = out(println) _
  def outPrint = out(print) _
  def prnln(text: String, color: Option[Color] = None) = outPrintln(text, color)
  def prn(text: String, color: Option[Color] = None) = outPrint(text, color)
  def apply(text: String, color: Option[Color] = None) = prnln(text, color)

}

object CourseraParser {

  import Coursera._

  private val RootElement = ".course-item-list-section-list"
  private val HeaderSubElement = "h3"
  private val ChapterElement = ".course-lecture-item-resource"

  def parsePage(st: String): List[List[String]] = {
    CourseraOutput("Getting files list...")
    Jsoup.parse(st).select(RootElement).iterator().asScala.map {
      el => parseElement(el)
    }.toList
  }

  private def parseElement(el: Element): List[String] = {
    val header: String = el.previousElementSibling().select(HeaderSubElement).
      text().trim().replaceAll("\u00a0","")
    header :: (
      for (
        links: Element <- el.select(ChapterElement).iterator().asScala;
        link: Element <- links.select("a").iterator().asScala
      ) yield Http.urlDecode(link.attr("href"), DefaultCodePage)
    ).toList
  }

}

class CourseraOptions(arguments: Seq[String]) extends ScallopConf(arguments) {

  banner("""Usage: coursera [OPTIONS] [classname]
           |This program helps to download coursera class lectures
           |Options:
           |""".stripMargin)
  version("0.0.3 (c) 2013 Igor Nemilentsev")

  val username = opt[String](required = false, descr = "set username/email")
  val password = opt[String](required = false, descr = "set password")
  val filename = opt[String](required = false, descr = "set configuration file")
  val info = opt[Boolean](required = false,
    descr = "show available files and exit", default = Some(false))
  val force = opt[Boolean](required = false,
    descr = "force re-download files", default = Some(false), short = 'u')
  val pdf = opt[Boolean](required = false,
    descr = "get only pdf files", default = Some(false), short = 'm')
  val txt = opt[Boolean](required = false,
    descr = "get only txt files", default = Some(false))
  val srt = opt[Boolean](required = false,
    descr = "get only srt files", default = Some(false))
  val pptx = opt[Boolean](required = false,
    descr = "get only pptx files", default = Some(false))
  val mp4 = opt[Boolean](required = false,
    descr = "get only mp4 files", default = Some(false))
  val zip = opt[Boolean](required = false,
    descr = "get only zip files", default = Some(false))
  val directory = opt[String](required = false,
    descr = "set directory to save files. default is a current directory.",
    validate = a => new File(a).isDirectory)
  val chapter = opt[List[Int]](required = false,
    descr = "set lecture chapters by number. -c 1 2 3",
    validate = a => a.forall(_ > 0), short = 'c')
  val chapters = opt[List[Int]](required = false,
    descr = "set lecture chapters by period. -v 1 5 or -v 2." +
      " It will download 1,2,3,4,5 chapters or in second" +
      " case all chapters from number 2",
    validate = a => a.forall(_ > 0) && (a.length == 2 || a.length == 1),
    short = 'v')
  val classname= trailArg[String](required = false,
    descr = "set Coursera class name")
  mutuallyExclusive(username, filename)
  mutuallyExclusive(password, filename)
  codependent(username, password)
}


object Coursera {

  private val AuthCookieName        = "CAUTH"
  private val MaestroLoginFlagName  = "maestro_login_flag"
  private val CsrfTokenCookieName   = "csrf_token"
  private val PasswordFieldName     = "password"
  private val UsernameFieldName     = "email"
  private val WebRequestFieldName   = "webrequest"
  private val ConnectionTimeout     = 3600 * 1000
  private val OkResponseCode        = 200
  private val RedirectResponseCode  = 302
  private val LectureUrl            =
    "https://class.coursera.org/%s/lecture/index"
  private val loginUrl              =
    "https://accounts.coursera.org/api/v1/login"
  private val ReferrerUrl           = "https://accounts.coursera.org/signin"
  private val ClassAuthUrl          =
    "https://class.coursera.org/%s/auth/auth_redirector?" +
      "type=login&subtype=normal"
  private val cookiesClassNames     = List(AuthCookieName, MaestroLoginFlagName)
  private val cookiesSessionNames   = List(
    AuthCookieName, CsrfTokenCookieName, MaestroLoginFlagName)
  private val requestHeaders        = List(
    ("Accept", "*/*"), ("User-Agent", "scalaj-http"))
  private val courSeraHttpOptions   = List(
    HttpOptions.connTimeout(ConnectionTimeout),
    HttpOptions.readTimeout(ConnectionTimeout),
    HttpOptions.allowUnsafeSSL)
  private val supportedExt = List("txt", "pdf", "srt", "mp4", "pptx", "zip")
  private val fileExtRegexpString =
    ".+\\.(%s)$".format(supportedExt.mkString("|"))
  val DefaultCodePage       = "UTF-8"

  def getChapterPeriod(chapters: Option[List[Int]]) = chapters match {
    case ls @ Some(List(_)) => ls
    case Some(f :: s :: Nil) => Some((f to s).toList)
    case _ => None
  }

  def mergeChapters(
      chapters: Option[List[Int]],
      periodChapters: Option[List[Int]]) = {
    val union = chapters.get union getChapterPeriod(periodChapters).get
    Some(union.toSet.toList.sorted)
  }

  def getCredentials(
      conf: CourseraOptions,
      filename: Option[String],
      className: Option[String]) = {
    if (conf.username.isEmpty || conf.password.isEmpty || className.isEmpty) {
      CourseraConfig.readConfigs(filename)
    } else {
      (Some(conf.username()), Some(conf.password()), className)
    }
  }

  def getAllChapters(
      chapters: Option[List[Int]],
      periodChapters: Option[List[Int]]) = {
    if (chapters.isEmpty && periodChapters.isEmpty) None
    else if (chapters.nonEmpty && periodChapters.isEmpty) chapters
    else if (chapters.isEmpty && periodChapters.nonEmpty)
      getChapterPeriod(periodChapters)
    else mergeChapters(chapters, periodChapters)
  }

  def cookiesToMap(st: String): Map[String, String] = {
    st.split(";\\s+").map(_.split("=", 2)).collect {
      case Array(a, b) => a -> b
    }.toMap
  }

}


class Coursera(
      course: String,
      username: String,
      password: String,
      directory: String,
      optionsMap: Map[String, Boolean]) {

  import Coursera._

  private val txt                   = optionsMap.getOrElse("txt", false)
  private val srt                   = optionsMap.getOrElse("srt", false)
  private val pdf                   = optionsMap.getOrElse("pdf", false)
  private val mp4                   = optionsMap.getOrElse("mp4", false)
  private val pptx                  = optionsMap.getOrElse("pptx", false)
  private val zip                   = optionsMap.getOrElse("zip", false)
  private val update                = optionsMap.getOrElse("update", false)
  private val LOG                   = Logger.getLogger("coursera")
  private lazy val getLectureUrl    = String.format(LectureUrl, course)
  private lazy val getClassAuthUrl  = String.format(ClassAuthUrl, course)
  def isTxt(filename: String)       = txt && filename.endsWith(".txt")
  def isPdf(filename: String)       = pdf && filename.endsWith(".pdf")
  def isSrt(filename: String)       = srt && filename.endsWith(".srt")
  def isMp4(filename: String)       = mp4 && filename.endsWith(".mp4")
  def isPptx(filename: String)      = pptx && filename.endsWith(".pptx")
  def isZip(filename: String)       = zip && filename.endsWith(".zip")

  lazy val getCsrfToken: String = {
    Http(getLectureUrl).options(courSeraHttpOptions).
      asCodeHeaders._2.get("Set-Cookie") match {
      case Some(list: List[String]) => cookiesToMap(list.head).
        getOrElse(CsrfTokenCookieName, "")
      case _ => ""
    }
  }

  def prepareLoginHeaders(csrf: String): List[(String,String)] =
    List(
      ("Referer", ReferrerUrl),
      ("X-CSRFToken", csrf),
      ("Cookie", "csrftoken=" + csrf)
    ) ::: requestHeaders

  def prepareLoginParams: List[(String, String)] =
    List(
      (UsernameFieldName, username),
      (PasswordFieldName, password),
      (WebRequestFieldName, "true")
    )

  def filterCookies(cookies: List[String], filter:List[String]): String = {
    val cookiesMap = cookiesToMap(cookies mkString "; ").
      filterKeys(filter.contains)
    cookiesMap.map {
      case (key, value) => "%s=%s" format(key, value)
    } mkString "; "
  }

  def prepareClassAuthHeaders: List[(String, String)] =
    List(
      ("Cookie", filterCookies(getAuthCookie, cookiesClassNames))
    ) ::: requestHeaders

  lazy val sessionHeaders: List[(String, String)] = {
    val sh = filterCookies(
      getAuthCookie ::: getSessionCookie, cookiesSessionNames)
    List(("Cookie", sh)) ::: requestHeaders
  }

  lazy val getAuthCookie: List[String] = {
    Http.post(loginUrl).headers(prepareLoginHeaders(getCsrfToken)).
      options(courSeraHttpOptions).
      params(prepareLoginParams).
      asCodeHeaders._2.get("Set-Cookie") match {
        case Some(lst) => lst
        case _ => Nil
    }
  }

  def getSessionCookie: List[String] = {
    CourseraOutput("Authenticating...")
    val response = Http(getClassAuthUrl).headers(prepareClassAuthHeaders).
      options(courSeraHttpOptions).
      asCodeHeaders
    response._2.get("Set-Cookie") match {
      case Some(x) => x
      case _ => throw new CourseraException(
        "Cannot login to coursera. Check email and password.")
    }
  }

  def getClassPage: String =
    Http(getLectureUrl).headers(sessionHeaders).
      options(courSeraHttpOptions).asString

  def getFilesList(chapters: Option[List[Int]]): List[(List[String], Int)] = {
    val files = CourseraParser.parsePage(getClassPage).zipWithIndex
    chapters match {
      case Some(List(n)) => files.filter(x => x._2 + 1 >= n).toList
      case Some(list) => files.filter(x => list.contains(x._2 + 1)).toList
      case None => files.toList
    }
  }

  def needDownloadFile(file: File, size: Long): Boolean = update ||
    !file.exists() || size != -1 && file.length != size

  def downloadFile(
      file: File,
      url: String,
      session: String,
      size: Long): (Boolean, String) = {
    if (needDownloadFile(file, size)) {
      new Downloader(file.getCanonicalPath, url, session)()
      (true, file.getName)
    } else (false, file.getName)
  }

  def downloadChapters(files: Iterable[(String, List[(Int, String, String)])]) {
    CourseraOutput("Starting download files...")
    files.foreach { case(name, list) => downloadChapter(name, list) }
  }

  def downloadChapter(
      chapterDirectory: String,
      chapter: List[(Int, String, String)]) {
    val dir = new File(directory, chapterDirectory)
    if (!dir.exists && !dir.isDirectory && !dir.mkdirs) {
      LOG.log(Level.SEVERE, "Cannot create directory " + dir.getCanonicalPath)
    } else {
      val session = sessionHeaders(0)._2
      val tasks: List[Future[(Boolean, String)]] = chapter.map {
        case(size, name, url) => future {
          downloadFile(new File(dir, name), url, session, size)
        }
      }
      tasks.foreach(f = f => {
        f onComplete {
          case Success((true, filename)) =>
            terminalWaitActor ! Message(filename, state = true)
          case Success((false, filename)) =>
            terminalWaitActor ! Message(filename, state = false)
          case Failure(t) =>
            CourseraOutput("An error has occurred: %s => %s" format(t.getMessage, t.getStackTraceString))
        }
      })
      val futures: Future[List[(Boolean, String)]] = Future.sequence(tasks)
      Await.result(futures, Duration.Inf)
    }
  }

  def showChapter(
      index: Int,
      chapterDirectory: String,
      chapter: List[(Int, String, String)]) {
    CourseraOutput.prn("#%s: " format index + 1, Some(Color.BLUE))(
      chapterDirectory, Some(Color.RED))
    println()
    val dir = new File(directory, chapterDirectory).getCanonicalPath
    chapter.zipWithIndex.foreach {
      case ((size, name, url), i) =>
        val file = new File(dir, name)
        if (!file.exists()) {
          CourseraOutput("%s: %s" format(i, name), Some(Color.GREEN))
        } else if (size != -1 && file.length != size) {
          CourseraOutput("%s: %s" format(i, name), Some(Color.BLUE))
        } else {
          CourseraOutput("%s: %s" format(i, name), Some(Color.CYAN))
        }
    }
    println()
  }

  def showChapters(files: Map[Int, (String, List[(Int, String, String)])]) {
    files.toIndexedSeq.sortBy(_._1) foreach {
      case (index, (dir, list)) => showChapter(index, dir, list)
    }
  }

  def parseContentDisposition(value: String): String = {
    val mp = value.split(";\\s+").map(x => x.split("=", 2)).collect {
      case Array(z, y) => z -> y
    }.toMap
    val filename = mp.getOrElse("filename", "").
      replaceAll("\"*$|^\"*", "").replaceAll("\'", "")
    URLDecoder.decode(filename, DefaultCodePage)
  }

  def parseUrlFilename(url: String): String = url.split("/").toList.last

  def getContentDisposition(headers: Map[String, List[String]]) = {
    headers.get("Content-Disposition") match {
      case Some(List(value)) => Option(parseContentDisposition(value))
      case _ => None
    }
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
        params(key) = values.toList
        values
      }
    }
    params.toMap
  }

  def getFileNameFromUrl(url: String) = {
    val slashIndex = url.lastIndexOf("/")
    val qIndex = url.lastIndexOf("?")
    if (qIndex > slashIndex) {
      //if has parameters
      url.substring(slashIndex + 1, qIndex)
    } else url.substring(slashIndex + 1)
  }

  def getContentDispositionFromLocation(url: String) =
    parseUriParameters(url).get("response-content-disposition") match {
      case Some(List(value)) => parseContentDisposition(value)
      case _ => getFileNameFromUrl(url)
    }

  def getFileData(url: String): Option[(Int, String, String)] = {
    // returns content-length, filename and url
    val response = Http(url).
      headers(sessionHeaders).
      options(courSeraHttpOptions ::: List(HttpOptions.method("HEAD"))).
      option(_.setInstanceFollowRedirects(false)).asCodeHeaders
    response match {
      case (OkResponseCode, headers) =>
        Some((
          headers.getOrElse("Content-Length", List("-1"))(0).toInt,
          getContentDisposition(headers) match {
            case Some(value) => value
            case _ => parseUrlFilename(url)
          },
          url
        ))
      case (RedirectResponseCode, headers) =>
        val location = headers.get("Location").get(0)
        val (code, resHeaders) = Http(location).headers(sessionHeaders).
          options(courSeraHttpOptions).asCodeHeaders
        if (code >= 300) {
          LOG.log(Level.SEVERE,
            s"Cannot get file data for url: $location . Response code: $code")
          None
        } else {
          val contentDisposition = getContentDisposition(headers)
          val contentLength =
            resHeaders.getOrElse("Content-Length", List("-1"))(0).toInt
          val filename = contentDisposition match {
            case Some(value) => value
            case _ => getContentDispositionFromLocation(location)
          }
          if (filename.isEmpty) {
            LOG.log(Level.WARNING, s"Cannot get file name from url: $location")
            None
          }
          else Some((contentLength, filename, location))
        }
      case (code, headers) =>
        LOG.log(Level.SEVERE,
          "Cannot get file data for url: %s . Response code: %s"
            format(url, code))
        Some((
          -1,
          URLDecoder.decode(getFileNameFromUrl(url), DefaultCodePage),
          url
        ))
    }
  }

  def getLectureData(
        files: List[String],
        title: String): List[(Int, String, String)] = {
    CourseraOutput.prn(
      "[%s] - " format title, Some(Color.RED))("Getting files data...")
    val tasks: List[Future[Option[(Int, String, String)]]] =
      files.map { url => future { getFileData(url) }}
    val futures: Future[List[Option[(Int, String, String)]]] =
      Future.sequence(tasks)
    Await.result(futures, Duration.Inf).toList.flatten
  }

  val notSetAny =  List(txt, pdf, mp4, srt, pptx, zip).forall(x => !x)

  def isValidFileExt(filename: String) = filename matches fileExtRegexpString

  def filterFile(filename: String): Boolean = {
    isValidFileExt(filename) &&
      (
        notSetAny ||
        isTxt(filename) ||
        isMp4(filename) ||
        isPdf(filename) ||
        isSrt(filename) ||
        isPptx(filename) ||
        isZip(filename)
      )
  }

  def getLecturesData(files: List[(List[String], Int)]):
      Map[Int, (String, List[(Int, String, String)])] = {
    files.map {
      case ((title :: urls), number) =>
        (number, (title, getLectureData(urls, title).filter(x => filterFile(x._2))))
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
      case e: CourseraException =>
        CourseraOutput.prn(e.getMessage, Some(Color.RED))
      case e: Exception =>
        CourseraOutput("An error has occurred: %s => %s" format(e.getMessage, e.getStackTraceString))
    } finally {
      terminalWaitActor ! Stop
    }
  }

}

object terminalWaitActor extends Actor {

  var count = 0
  var pos = 0
  var clearCounter = 0
  var speedPerWheel = 10
  var filesDone = 0
  var size: Long = 0
  var time: Long = 0
  val positions = Array("-", "\\", "|", "/")
  val timeToSleep = 300

  def showProgress(timeDiff: Option[Long]) {
    pos = (pos + 1)  % 4
    clearCounter += 1
    val currentSize = if (timeDiff.isDefined) " [%.02fKB/s]" format
      (size * 1000 / 1024) / timeDiff.get.toFloat else ""
    val message = {
      val text = if (count > 0)
        "[%s]%s done: [%s/%s]".format(positions(pos), currentSize, filesDone, count)
      else "[%s]".format(positions(pos))
      "%s%s" format(text, "\b" * text.length)
    }
    CourseraOutput.prn(message)
    actor {
      Thread.sleep(timeToSleep)
      terminalWaitActor ! Progress
    }
  }

  def act() {
    showProgress(None)
    loop {
      react {
        case Process(sz) =>
          if (time == 0) time = System.currentTimeMillis
          size = size + sz
        case Progress =>
          val curTime = System.currentTimeMillis
          val diff = curTime - time
          showProgress(Some(diff))
          if ((clearCounter % speedPerWheel) == 0) {
            time = curTime
            size = 0
            clearCounter = 0
          }
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

  import org.streum.configrity.io.StandardFormat.ParserException

  private val defaultConfigFile = "coursera.conf"
  private val LOG = Logger.getLogger("coursera")


  def checkFileExists(file: File): Boolean = file.exists && file.isFile

  def getConfigs(filename: Option[String]): List[String] = {
    val list = List(
      new File(new File(".").getAbsolutePath, defaultConfigFile),
      new File(new File(
        System.getProperty("user.home")).getAbsolutePath, defaultConfigFile))
    (if (filename.isDefined) new File(filename.get) :: list else list).
      filter(checkFileExists).map(_.getCanonicalPath)
  }

  def readConfigs(filename: Option[String]) = {
    val data = mergeConfigs(getConfigs(filename) map parseConfig)
    (data(0), data(1), data(2))
  }

  def mergeConfigs(
      list: List[(Option[String], Option[String], Option[String])]) = {
    val (a, b, c) = list.unzip3
    List(a, b, c).map(_.collect { case Some(x) => x }).map {
      x => if (x.isEmpty) None else Some(x(0))
    }
  }

  def parseConfig(filename: String):
      (Option[String], Option[String], Option[String]) = {
    try {
      val config = Configuration.load(filename)
      (config.get[String]("username"),
        config.get[String]("password"),
        config.get[String]("classname"))
    } catch {
      case e: ParserException =>
        LOG.severe("Cannot parse file: %s" format filename)
        (None, None, None)
      case e: Exception =>
        LOG.severe("Something went wrong during processing file: %s => %s"
          format(filename, e))
        (None, None, None)
    }
  }

}

class Downloader(path: String, location: String, session: String) {

  private val Buffer = 1024 * 5
  private val LOG = Logger.getLogger("coursera")

  type Closable = { def close() }

  def using2[T <: Closable, A <: Closable]
      (in: T, out: A, f: => (Throwable) => Unit)
      (block: => (T, A) => Unit) {
    try {
      block(in, out)
    } catch {
      case ex: Throwable => f(ex)
    } finally {
      in.close()
      out.close()
    }
  }
  def getConnectionStream = {
    val url = new URL(location.replaceAll(" ", "%20"))
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.addRequestProperty("Cookie", session)
    new BufferedInputStream(connection.getInputStream)
  }
  def download() {
    using2(
        getConnectionStream,
        new BufferedOutputStream(new FileOutputStream(path)),
        e => LOG.log(Level.SEVERE, "Cannot download file: %s" format path, e)
      ) { (in, out) =>
      var count = 0
      val buffer: Array[Byte] = new Array[Byte](Buffer)
      while ({ count = in.read(buffer, 0, Buffer); count != -1}) {
        out.write(buffer, 0, count)
        terminalWaitActor ! Process(count)
      }
    }
  }
  def apply() {
    download()
  }
}

object Main {

  import Coursera._

  private val LOG = Logger.getLogger("coursera")

  def main(args: Array[String]) {

    val conf = new CourseraOptions(args)

    val className  = if (conf.classname.isEmpty) None
      else Some(conf.classname())
    val fileName   = if (conf.filename.isEmpty) None else Some(conf.filename())
    val chapters   = if (conf.chapter.isEmpty) None else Some(conf.chapter())
    val periodChapters = if (conf.chapters.isEmpty) None
      else Some(conf.chapters())
    val directory  = if (conf.directory.isEmpty) new File(".").getCanonicalPath
      else conf.directory()
    val optionsMap = Map(
      "info" -> conf.info(),
      "update" -> conf.force(),
      "pdf" -> conf.pdf(),
      "txt" -> conf.txt(),
      "srt" -> conf.srt(),
      "mp4" -> conf.mp4(),
      "pptx" -> conf.pptx(),
      "zip" -> conf.zip()
    )

    getCredentials(conf, fileName, className) match {
      case (Some(username), Some(password), Some(classname)) =>
        new Coursera(
          classname,
          username,
          password,
          directory,
          optionsMap)(getAllChapters(chapters, periodChapters))
      case (Some(username), Some(password), None) =>
        className match {
          case Some(classname) =>
            new Coursera(
              classname,
              username,
              password,
              directory,
              optionsMap)(getAllChapters(chapters, periodChapters))
          case _ =>
            LOG.info("Should be set classname parameter either" +
              " in a configuration file or in the command line")
            conf.printHelp()
            sys.exit(1)
      }
      case _ =>
        LOG.info("Should be set either a configuration" +
          " filename or an username and password")
        conf.printHelp()
        sys.exit(1)
    }
  }
}
