package by.trezor.coursera

import scala.collection.JavaConverters.asScalaIteratorConverter
import scalaj.http._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element


object CouseraPageParser {

  private final val RootElement = ".course-item-list-section-list"
  private final val HeaderSubElement = "h3"
  private final val ChapterElement = ".course-lecture-item-resource"

  def parsePage(st: String): List[List[String]] = {
    (for (el: Element <- Jsoup.parse(st).select(RootElement).iterator().asScala) yield parseElement(el)).toList
  }

  private def parseElement(el: Element): List[String] = {
    val header: String = el.previousElementSibling().select(HeaderSubElement).text().toLowerCase.
      replaceAll("(?i).*Week\\s*(\\d+):\\s*", "$1.").replaceAll("[\\s,]+", "-").trim()
    header :: (
      for (
        links: Element <- el.select(ChapterElement).iterator().asScala;
        link: Element <- links.select("a").iterator().asScala
      ) yield Http.urlDecode(link.attr("href"))
    ).toList
  }

}


class Coursera(course: String, username: String, password: String) {

  private final val SessionIdCookieName = "sessionid"
  private final val SessionCookieName = "session"
  private final val MaestroLoginCookieName = "maestro_login"
  private final val MaestroLoginFlagName = "maestro_login_flag"
  private final val CsrfTokenCookieName = "csrf_token"
  private final val ConnectionTimeout = 20000
  private final val LectureUrl = "http://class.coursera.org/%s/lecture/index"
  private final val loginUrl = "https://www.coursera.org/maestro/api/user/login"
  private final val RefererUrl = "https://www.coursera.org"
  private final val ClassAuthUrl = "https://class.coursera.org/%s/auth/auth_redirector?" +
    "type=login&subtype=normal&email=&visiting=%s"

  lazy val getLectureUrl: String = String.format(LectureUrl, course) 
  
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

  val couseraHttpOptions: List[HttpOptions.HttpOption] =
    List(HttpOptions.connTimeout(ConnectionTimeout), HttpOptions.readTimeout(ConnectionTimeout))

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
    List(("email_address", username), ("password", password))

  def prepareClassAuthHeaders: List[(String, String)] = {
    val classAuthCookie = for (
      map <- getAuthCookie.map(cookiesToMap(_));
      (name, value) <- map
      if (cookiesNames.contains(name)))
    yield name + "=" + value
    List(("Cookie", classAuthCookie mkString "; "))
  }

  def prepareSessionHeaders(session: String): List[(String, String)] =
    List(("Cookie", "session=" + session))

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

  def getFilesList: List[List[String]] = {
    CouseraPageParser.parsePage(getClassPage)
  }

  def apply(x: Nothing*): List[List[String]] = getFilesList

}

object Main {
  def main(args: Array[String]) {
    val coursera = new Coursera("", "", "")
    println(coursera())
  }
}
