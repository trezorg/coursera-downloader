package by.trezor.coursera

import scalaj.http._

class Coursera(course: String, username: String, password: String) {

  private final val SessionIdCookieName = "sessionid"
  private final val SessionCookieName = "session"
  private final val MaestroLoginCookieName = "maestro_login"
  private final val MaestroLoginFlagName = "maestro_login_flag"
  private final val CsrfTokenCookieName = "csrf_token"
  private final val ConnectionTimeout = 10000
  private final val LectureUrl = "http://class.coursera.org/%s/lecture/index"
  private final val loginUrl = "https://www.coursera.org/maestro/api/user/login"
  private final val RefererUrl = "https://www.coursera.org"
  private final val ClassAuthUrl = "https://class.coursera.org/%s/auth/auth_redirector?type=login&subtype=normal&email=&visiting=%s"

  lazy val getLectureUrl: String = String.format(LectureUrl, course) 
  
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
    Http(getLectureUrl).option(HttpOptions.connTimeout(ConnectionTimeout)).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie") match {
        case Some(list: List[String]) => cookiesToMap(list.head) getOrElse(CsrfTokenCookieName, "")
        case _ => ""
    }
  }

  def prepareLoginHeaders(csrf: String): List[(String,String)] = {
    List(("Referer", RefererUrl), ("X-CSRFToken", csrf), ("Cookie", "csrftoken=" + csrf))
  }

  def prepareLoginParams(username: String, password: String): List[(String, String)] = {
    List(("email_address", username), ("password", password))
  }

  lazy val getAuthCookie: List[String] = {
    Http.post(loginUrl).headers(prepareLoginHeaders(getCsrfToken)).
      option(HttpOptions.connTimeout(ConnectionTimeout)).
      params(prepareLoginParams(username, password)).
      asHeadersAndParse(Http.readString)._2.get("Set-Cookie").head
  }

  def prepareClassAuthHeaders: List[(String, String)] = {
   val classAuthCookie = for (
        map <- getAuthCookie.map(cookiesToMap(_));
        (name, value) <- map
        if (cookiesNames.contains(name)))
        yield name + "=" + value
    List(("Cookie", classAuthCookie mkString "; "))
  }

  def getSessionCookie: String = {
    Http(getClassAuthUrl).headers(prepareClassAuthHeaders).
      option(HttpOptions.connTimeout(ConnectionTimeout)).
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

  def apply(x: Nothing*): String = getSessionCookie

}

object Main {
  def main(args: Array[String]) {
    val coursera = new Coursera("", "", "")
    println(coursera())
  }
}
