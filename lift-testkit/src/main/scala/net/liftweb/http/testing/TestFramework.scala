package net.liftweb.http.testing

import net.liftweb.util.Helpers._
import net.liftweb.util._
import scala.xml.{NodeSeq, Text, XML, Elem}
import java.util.{Map => JavaMap, Set => JavaSet, Iterator => JavaIterator, List => JavaList}
import java.util.regex.Pattern
import java.io.IOException
import org.apache.commons.httpclient._
import methods._

trait TestFramework {
  def baseUrl: String
  // def runner: TestRunner
  def tests: List[Item]
  def buildRunner: TestRunner
  
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
  
  class TestHandler(res: Response) {
    def then (f: Response => Response): Response = f(res)
    def also (f: Response => Any): Response = {f(res); res}
  }
  implicit def reqToHander(in: Response): TestHandler = new TestHandler(in)
  
  
  def get(url: String, params: (String, Any)*): Response = get(url, authFunc, Nil, params :_*)
  def post(url: String, params: (String, Any)*): Response = post(url, authFunc, Nil, params :_*)
  
  def getCookie(headers: List[(String, String)], respHeaders: Map[String, List[String]]): Can[String] = {
    val ret = (headers.filter{case ("Cookie", _) => true; case _ => false}.map(_._2) ::: respHeaders.get("Set-Cookie").toList.flatMap(x => x)) match {
      case Nil => Empty
      case "" :: Nil => Empty
      case "" :: xs => Full(xs.mkString(","))
      case xs => Full(xs.mkString(","))
    }
    
    ret
  }
  
  protected lazy val httpClient = new HttpClient(new MultiThreadedHttpConnectionManager)
  
  private def slurpApacheHeaders(in: Array[Header]): Map[String, List[String]] = {
    val headerSet: List[(String, String)] = for (e <- in.toList ; h <- e.getElements) yield (h.getName -> h.getValue)
        
    def stringPairAccumulator(in: List[(String, String)], acc: Map[String, List[String]]): Map[String, List[String]] = 
      in match {
        case Nil => acc
        
        case (name, value) :: xs =>
          val oldList = acc.getOrElse(name, Nil)
          val newList = value :: oldList
          val newAcc = acc + (name -> newList)
          stringPairAccumulator(xs, newAcc)
      }
    stringPairAccumulator(headerSet, Map.empty)
  }
  
  def get(url: String, authFunc: Can[String => (String, String)], headers: List[(String, String)], faux_params: (String, Any)*): Response = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val fullUrl = url + (params.map(v => urlEncode(v._1)+"="+urlEncode(v._2)).mkString("&") match {case s if s.length == 0 => ""; case s => "?"+s})
    val getter = new GetMethod(baseUrl + fullUrl)
    for ((name, value) <- headers) getter.setRequestHeader(name, value)
        
    val ret = try {
      (baseUrl + fullUrl, httpClient.executeMethod(getter)) match {
        // case (_, 401) if authFunc.isDefined => 
        case (server, responseCode) => 
          val respHeaders = slurpApacheHeaders(getter.getResponseHeaders)
          new HttpResponse(responseCode, getter.getStatusText, respHeaders, getter.getResponseBody, getCookie(headers, respHeaders))
        
        /*
        case (_, u: HttpURLConnection) 
        if u.getResponseCode == HttpURLConnection.HTTP_UNAUTHORIZED && 
        !authFunc.isEmpty =>
        val respHeaders = snurpHeaders(u.getHeaderFields)
        (for (hdrs <- Can(respHeaders.get("WWW-Authenticate"));
        hdr <- hdrs.headOption;
        func <- authFunc) yield func(hdr)) match {
          case Full((name, pwd)) => get(url, Empty, 
          "Authorization" -> ("Basic "+buildAuth(name, pwd)) :: headers, faux_params :_*)
          case _ => get(url, Empty, headers, faux_params :_*)
        }
        
        case (_, u: HttpURLConnection) =>
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
        val respHeaders = snurpHeaders(u.getHeaderFields)
        
        case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server, Empty) 
        */
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + fullUrl, Full(e))
    }
    ret
  }
  
  def buildAuth(userName: String, pwd: String): String = base64Encode((userName+":"+pwd).getBytes("UTF-8"))
  
  def post(url: String, authFunc: Can[String => (String, String)], headers: List[(String, String)], faux_params: (String, Any)*): Response = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val poster = new PostMethod(baseUrl + url)
    for ((name, value) <- headers) poster.setRequestHeader(name, value)
    for ((name, value) <- params) poster.setParamater(name, value)
    
    val ret = try {
      (baseUrl + url, httpClient.executeMethod(poster)) match {        
        case (_, u: HttpURLConnection) =>
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
        u.setDoOutput(true)
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
        u.setRequestProperty("Content-Length", paramByte.length.toString)
        u.getOutputStream.write(paramByte)
        
        val respHeaders = snurpHeaders(u.getHeaderFields)
        if (u.getResponseCode == HttpURLConnection.HTTP_UNAUTHORIZED && 
        !authFunc.isEmpty) {
          val respHeaders = snurpHeaders(u.getHeaderFields)
          (for (hdrs <- Can(respHeaders.get("WWW-Authenticate"));
          hdr <- hdrs.headOption;
          func <- authFunc) yield func(hdr)) match {
            case Full((name, pwd)) => post(url, Empty, 
            "Authorization" -> ("Basic "+buildAuth(name, pwd)) :: headers, faux_params :_*)
            
            case _ => new HttpResponse(u.getResponseCode, u.getResponseMessage, 
            respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
          }
        } else {
          new HttpResponse(u.getResponseCode, u.getResponseMessage, 
          respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
        }
        case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server, Empty) 
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + url, Full(e))
    }
    ret
  }
  
  def post(url: String, authFunc: Can[String => (String, String)], body: NodeSeq, headers: List[(String, String)]): Response = {
    val paramByte = body.toString.getBytes("UTF-8")
    val ret = try {
      (baseUrl + url, new URL(baseUrl + url).openConnection) match {
        case (_, u: HttpURLConnection) =>
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
        u.setDoOutput(true)
        u.setRequestMethod("POST")
        u.setRequestProperty("Content-Type", "text/xml")
        u.setRequestProperty("Content-Length", paramByte.length.toString)
        u.getOutputStream.write( paramByte)
        val respHeaders = snurpHeaders(u.getHeaderFields)
        
        if (u.getResponseCode == HttpURLConnection.HTTP_UNAUTHORIZED && 
        !authFunc.isEmpty) {
          val respHeaders = snurpHeaders(u.getHeaderFields)
          (for (hdrs <- Can(respHeaders.get("WWW-Authenticate"));
          hdr <- hdrs.headOption;
          func <- authFunc) yield func(hdr)) match {
            case Full((name, pwd)) => post(url, Empty, body,
            "Authorization" -> ("Basic "+buildAuth(name, pwd)) :: headers)
            
            case _ => new HttpResponse(u.getResponseCode, u.getResponseMessage, 
            respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
          }
        } else {
          new HttpResponse(u.getResponseCode, u.getResponseMessage, 
          respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
        }
        case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server, Empty) 
      }
    } catch {
      case e: IOException => new CompleteFailure(baseUrl + url, Full(e))
    }
    ret
  }
  
  trait Response {
    def assertSuccess = this
    def xml: Elem = <xml:group />
    
    def assert(f: => Boolean, msg: String): Response = {
      TestFramework.this.assertFunc(msg,{() =>
        if (!f) throw new TestFailureError(msg)
        this
      })
    }
    def headers: Map[String, List[String]] = Map.empty
    def assertTag(tag: String, msg:String) = assert((xml \\ tag).length > 0, msg)
    def code: Int = -1
    def msg: String = ""
    def cookie: Can[String] = Empty
    val body: Array[Byte] = new Array(0)
    lazy val bodyAsString = new String(body, "UTF-8")
    def get(url: String, params: (String, Any)*) = TestFramework.this.get(url, authFunc, cookie.map( ("Cookie", _) ).toList , params:_*)
    
    def post(url: String, params: (String, Any)*) = TestFramework.this.post(url, authFunc, cookie.map( ("Cookie", _) ).toList, params:_*)
    
    def post(url: String, body: NodeSeq) = TestFramework.this.post(url, authFunc, body, cookie.map( ("Cookie", _) ).toList)
    
    def authFunc: Can[String => (String, String)] = Empty
  }
  
  def authFunc: Can[String => (String, String)] = Empty
  
  class HttpResponse(override val code: Int, override val msg: String, override val headers: Map[String, List[String]],
  override val body: Array[Byte], override val cookie: Can[String]) extends Response {
    
    override def assertSuccess = assert(code == 200, "Not an HTTP success")
    override lazy val xml = XML.load(new java.io.ByteArrayInputStream(body))
  }
  
  class CompleteFailure(val serverName: String, val exception: Can[Throwable]) extends Response {
    override def assertSuccess = assert(false, "Failed to connect to server: "+serverName)
    override def toString = serverName + (exception.map(e => " Exception: " + e.getMessage) openOr "") 
  }
  
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
