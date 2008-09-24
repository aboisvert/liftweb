package net.liftweb.http.testing

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.scala.xml.{NodeSeq, Text, XML, Elem}
import _root_.java.util.{Map => JavaMap, Set => JavaSet, Iterator => JavaIterator, List => JavaList}
import _root_.java.util.regex.Pattern
import _root_.java.io.IOException
import _root_.org.apache.commons.httpclient._
import methods._
import _root_.java.io.OutputStream
import _root_.org.apache.commons.httpclient.auth.AuthScope

trait GetPoster {
  def baseUrl: String

  private def slurpApacheHeaders(in: Array[Header]):
  Map[String, List[String]]
  = {
    val headerSet: List[(String, String)] =
      for (e <- in.toList ; h <- e.getElements)
      yield (h.getName -> h.getValue);

    headerSet.foldLeft[Map[String, List[String]]](Map.empty)((acc, e) =>
      acc + (e._1 -> (e._2 :: acc.getOrElse(e._1, Nil))))
  }


  def get(url: String, httpClient: HttpClient,
	  headers: List[(String, String)],
	  faux_params: (String, Any)*): Response
  = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val fullUrl = url + (params.map(v => urlEncode(v._1)+"="+urlEncode(v._2)).mkString("&") match {case s if s.length == 0 => ""; case s => "?"+s})
    val getter = new GetMethod(baseUrl + fullUrl)
    for ((name, value) <- headers) getter.setRequestHeader(name, value)

    val ret = try {
      (baseUrl + fullUrl, httpClient.executeMethod(getter)) match {
        case (server, responseCode) =>
          val respHeaders = slurpApacheHeaders(getter.getResponseHeaders)

        new HttpResponse(baseUrl,
			 responseCode, getter.getStatusText,
			 respHeaders, getter.getResponseBody,
			 httpClient)
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + fullUrl, Full(e))
    } finally {
      getter.releaseConnection
    }
    ret
  }

  def post(url: String, httpClient: HttpClient,
	   headers: List[(String, String)],
	   faux_params: (String, Any)*): Response
  = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val poster = new PostMethod(baseUrl + url)
    for ((name, value) <- headers) poster.setRequestHeader(name, value)
    for ((name, value) <- params) poster.setParameter(name, value)

    val ret = try {
      (baseUrl + url, httpClient.executeMethod(poster)) match {
	case (server, responseCode) =>
	  val respHeaders = slurpApacheHeaders(poster.getResponseHeaders)
          new HttpResponse(baseUrl,
			   responseCode, poster.getStatusText,
			   respHeaders, poster.getResponseBody,
			   httpClient)
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + url, Full(e))
    } finally {
      poster.releaseConnection
    }
    ret
  }

  def post(url: String, httpClient: HttpClient,
	   headers: List[(String, String)],
	   body: NodeSeq): Response
  = {
    val poster = new PostMethod(baseUrl + url)
    for ((name, value) <- headers) poster.setRequestHeader(name, value)
    poster.setRequestEntity(new RequestEntity {
      private val bytes = body.toString.getBytes("UTF-8")

      def getContentLength() = bytes.length
      def getContentType() = "text/xml"
      def isRepeatable() = true
      def writeRequest(out: OutputStream) {
	out.write(bytes)
      }
    })

    val paramByte = body.toString.getBytes("UTF-8")
    val ret = try {
      (baseUrl + url, httpClient.executeMethod(poster)) match {
	case (server, responseCode) =>
	  val respHeaders = slurpApacheHeaders(poster.getResponseHeaders)
          new HttpResponse(baseUrl,
			   responseCode, poster.getStatusText,
			   respHeaders, poster.getResponseBody,
			   httpClient)
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + url, Full(e))
    } finally {
      poster.releaseConnection
    }
    ret
  }
}


trait TestFramework extends GetPoster {
  def baseUrl: String
  // def runner: TestRunner
  def tests: List[Item]
  def buildRunner: TestRunner

  /*
  private var assertFunc: (String, () => Response) => Response = _
  def runTests = synchronized {
    val (runner, tAssert) = buildRunner.setup[Response](tests)
    try {
      assertFunc = tAssert
      runner()
    } finally {
      assertFunc = null
    }
  }
  */

  class TestHandler(res: Response) {
    def then (f: Response => Response): Response = f(res)
    def also (f: Response => Any): Response = {f(res); res}
  }
  implicit def reqToHander(in: Response): TestHandler = new TestHandler(in)


  def get(url: String, buildHttpClient: () => HttpClient,
	  params: (String, Any)*): Response =
    get(url, buildHttpClient(), Nil, params :_*)

  def post(url: String, buildHttpClient: () => HttpClient,
	   params: (String, Any)*): Response =
    post(url, buildHttpClient(), Nil, params :_*)

  def buildNoAuthClient =
    new HttpClient(new SimpleHttpConnectionManager(false))

  def buildBasicAuthClient(name: String, pwd: String) = {
    val ret = new HttpClient(new SimpleHttpConnectionManager(false))
    val defaultcreds = new UsernamePasswordCredentials(name, pwd)
    ret.getState().setCredentials(AuthScope.ANY, defaultcreds)

    ret
  }


  def getCookie(headers: List[(String, String)],
		respHeaders: Map[String, List[String]]): Can[String]
  =
    {
      val ret = (headers.filter{case ("Cookie", _) => true; case _ => false}.
		 map(_._2) :::
		 respHeaders.get("Set-Cookie").toList.flatMap(x => x)) match {
		   case Nil => Empty
		   case "" :: Nil => Empty
		   case "" :: xs => Full(xs.mkString(","))
		   case xs => Full(xs.mkString(","))
		 }

      ret
    }

  // protected lazy val httpClient = new HttpClient(new MultiThreadedHttpConnectionManager)

  def fork(cnt: Int)(f: Int => Any) {
    val threads = for (t <- (1 to cnt).toList) yield {
      val th = new Thread(new Runnable{def run {f(t)}})
      th.start
      th
    }

    def waitAll(in: List[Thread]) {
      in match {
        case Nil =>
        case x :: xs => x.join; waitAll(xs)
      }
    }

    waitAll(threads)
  }

  type CRK = JavaList[String]
  implicit def jitToIt[T](in: JavaIterator[T]): Iterator[T] = new Iterator[T] {
    def next: T = in.next
    def hasNext = in.hasNext
  }

  private def snurpHeaders(in: JavaMap[String, CRK]): Map[String, List[String]] = {
    def morePulling(e: JavaMap.Entry[String, CRK]): (String, List[String]) = {
      e.getValue match {
        case null => (e.getKey, Nil)
        case a => (e.getKey, a.iterator.toList)
      }
    }

    Map(in.entrySet.iterator.toList.filter(e => (e ne null) && (e.getKey != null)).map(e => morePulling(e)) :_*)
  }
}

object TestHelpers {
  /**
  * Get the function name given a particular comet actor name
  *
  * @param cometName the name (default prefix) for the comet actor
  * @param body the body of the response
  *
  * @return the name of the JSON function associated with the Comet actor
  */
  def jsonFuncForCometName(cometName: String, body: String): Can[String] = {
    val p = Pattern.compile("""JSON Func """+cometName+""" \$\$ (F[^ ]*)""")
    val m = p.matcher(body)
    if (m.find) Full(m.group(1))
    else Empty
  }


  /**
  * Given an HTML page, find the list of "lift_toWatch" names and values
  * These can be fed back into a comet request
  *
  * @param body the page body returned from an HTTP request
  *
  * @return a list of the "to watch" tokens and the last update tokens
  */
  def toWatchFromPage(body: String): List[(String, String)] = {
    val p = Pattern.compile("""lift_toWatch[ ]*\=[ ]*\{([^}]*)\}""")
    val rp = new REMatcher(body, p)
    val p2 = Pattern.compile("""(L[^\:]*)\: \'([0-9]*)""")

    for (it <- rp.capture;
    val q = new REMatcher(it, p2);
    em <- q.eachFound) yield (em(1), em(2))
  }

  /**
  * Given the body of a Comet response, parse for updates "lift_toWatch" values
  * and update the current sequence to reflect any updated values
  *
  * @param old the old toWatch sequence
  * @param body the body of the comet response
  *
  * @return the updated sequences
  */
  def toWatchUpdates(old: Seq[(String, String)], body: String): Seq[(String, String)] = {
    val p = Pattern.compile("""lift_toWatch\[\'(L[^\']*)\'] \= \'([0-9]*)""")
    val re = new REMatcher(body, p)
    val np = re.eachFound.foldLeft(Map(old :_*))((a, b) => a + ( (b(1), b(2))) )
    np.elements.toList
  }
}

  trait Response {
    //def assertSuccess = this
    def xml: Elem = <xml:group />

    /*
    def assert(f: => Boolean, msg: String): Response = {
      TestFramework.this.assertFunc(msg,{() =>
        if (!f) throw new TestFailureError(msg)
        this
      })
    }
    */

    def headers: Map[String, List[String]] = Map.empty
    // def assertTag(tag: String, msg:String) = assert((xml \\ tag).length > 0, msg)
  }

class HttpResponse(override val baseUrl: String,
		   val code: Int, val msg: String,
		   override val headers: Map[String, List[String]],
		   val body: Array[Byte],
		   val httpClient: HttpClient) extends
Response with GetPoster
{

  // override def assertSuccess = assert(code == 200, "Not an HTTP success")
  override lazy val xml = XML.load(new _root_.java.io.ByteArrayInputStream(body))

  lazy val bodyAsString = new String(body, "UTF-8")

  def get(url: String, params: (String, Any)*): Response =
    get(url, httpClient, Nil, params:_*)

  def post(url: String, params: (String, Any)*): Response =
    post(url, httpClient, Nil, params:_*)

  def post(url: String, body: NodeSeq): Response =
    post(url, httpClient, Nil, body)
}

class CompleteFailure(val serverName: String, val exception: Can[Throwable]) extends Response {
  // override def assertSuccess = assert(false, "Failed to connect to server: "+serverName)
  override def toString = serverName + (exception.map(e => " Exception: " + e.getMessage) openOr "")
}
