package net.liftweb.http.testing

import net.liftweb.util.Helpers._
import net.liftweb.util._
import scala.xml.{NodeSeq, Text, XML, Elem}
import java.net._

trait TestFramework {
  def baseUrl: String
  def runner: TestRunner
  def tests: List[Item]
  
  def runTests = runner.run(tests)
  
  class TestHandler(res: ReqRes) {
    def then (f: ReqRes => ReqRes): ReqRes = f(res)
    def also (f: ReqRes => Any): ReqRes = {f(res); res}
  }
  implicit def reqToHander(in: ReqRes): TestHandler = new TestHandler(in)
  

  def get(url: String, params: (String, Any)*): ReqRes = get(url, Nil, params :_*)
  def post(url: String, params: (String, Any)*): ReqRes = post(url, Nil, params :_*)
  
  def getCookie(headers: List[(String, String)], respHeaders: Map[String, List[String]]): Option[String] = {
    val ret = (headers.filter{case ("Cookie", _) => true; case _ => false}.map(_._2) ::: respHeaders.get("Set-Cookie").toList.flatMap(x => x)) match {
      case Nil => None
      case "" :: Nil => None
      case "" :: xs => Some(xs.mkString(","))
      case xs => Some(xs.mkString(","))
    }

    ret
  }
  
  def get(url: String,headers: List[(String, String)], faux_params: (String, Any)*): ReqRes = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val fullUrl = url + (params.map(v => urlEncode(v._1)+"="+urlEncode(v._2)).mkString("&") match {case s if s.length == 0 => ""; case s => "?"+s})  
    val ret = (baseUrl + fullUrl, new URL(baseUrl + fullUrl).openConnection) match {
      case (_, u: HttpURLConnection) =>
        
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
      val respHeaders = snurpHeaders(u.getHeaderFields)
      new HttpReqRes(u.getResponseCode, u.getResponseMessage, 
                     respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
      case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server) 
    }
    ret
  }
  def post(url: String,headers: List[(String, String)], faux_params: (String, Any)*): ReqRes = {
    val params = faux_params.toList.map(x => (x._1, x._2.toString))
    val paramStr = params.map(v => urlEncode(v._1)+"="+urlEncode(v._2)).mkString("&")
    val paramByte = paramStr.getBytes("UTF-8")
    val ret = (baseUrl + url, new URL(baseUrl + url).openConnection) match {
      case (_, u: HttpURLConnection) =>
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
      u.setDoOutput(true)
      u.setRequestMethod("POST")
      u.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
      u.setRequestProperty("Content-Length", paramByte.length.toString)
      u.getOutputStream.write( paramByte)
      val respHeaders = snurpHeaders(u.getHeaderFields)
      new HttpReqRes(u.getResponseCode, u.getResponseMessage, 
                     respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
      case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server) 
    }
    ret
  }
  
  def post(url: String,body: NodeSeq, headers: List[(String, String)]): ReqRes = {
    val paramByte = body.toString.getBytes("UTF-8")
    val ret = (baseUrl + url, new URL(baseUrl + url).openConnection) match {
      case (_, u: HttpURLConnection) =>
        headers.foreach(h => u.setRequestProperty(h._1, h._2))
      u.setDoOutput(true)
      u.setRequestMethod("POST")
      u.setRequestProperty("Content-Type", "text/xml")
      u.setRequestProperty("Content-Length", paramByte.length.toString)
      u.getOutputStream.write( paramByte)
      val respHeaders = snurpHeaders(u.getHeaderFields)
      new HttpReqRes(u.getResponseCode, u.getResponseMessage, 
                     respHeaders, readWholeStream(u.getInputStream), Map.empty, getCookie(headers, respHeaders))
      case (server, z) => Log.error("Tried to open an HTTP connection and got "+z); new CompleteFailure(server) 
    }
    ret
  }
  
  abstract class ReqRes {
    def assertSuccess = this
    def xml: Elem = <xml:group />
    
    def assert(f: => Boolean, msg: String): ReqRes = {
      TestFramework.this.runner.applyAssert(msg) {
        if (!f) throw new TestFailureError(msg)
        this
      }
    }
    def headers: Map[String, List[String]] = Map.empty
    def values: Map[String, String] = Map.empty
    def assertTag(tag: String, msg:String) = assert((xml \\ tag).length > 0, msg)
    def code: Int = -1
    def msg: String = ""
    def cookie: Option[String] = None
    val body: Array[Byte] = new Array(0)
    def get(url: String, params: (String, Any)*) = TestFramework.this.get(url, cookie.map( ("Cookie", _) ).toList , params:_*)

    def post(url: String, params: (String, Any)*) = TestFramework.this.post(url, cookie.map( ("Cookie", _) ).toList, params:_*)

    def post(url: String, body: NodeSeq) = TestFramework.this.post(url, body, cookie.map( ("Cookie", _) ).toList)
  }
  
  class HttpReqRes(override val code: Int,override val msg: String,override val headers: Map[String, List[String]],
                   override val body: Array[Byte],override val values: Map[String, String],override val cookie: Option[String]) extends ReqRes {
                     
                     override def assertSuccess = assert(code == 200, "Not an HTTP success")
                     private val _body = Lazy(XML.load(new java.io.ByteArrayInputStream(body)))
                     override def xml = _body.get
                   }
  
  class CompleteFailure(val serverName: String) extends ReqRes {
    override def assertSuccess = assert(false, "Failed to connect to server: "+serverName)
  }

  private def snurpHeaders(in: java.util.Map): Map[String, List[String]] = {
    def entryToPair(e: AnyRef): List[(String, List[String])] = {
      e match {
        case null => Nil
        case e: java.util.Map.Entry => morePulling(e)
        case _ => Nil
      }
    }
    
    def morePulling(e: java.util.Map.Entry): List[(String, List[String])] = {
      (e.getKey, e.getValue) match {
        case (null, _) => Nil
        case (s: String, null) => List((s, Nil))
        case (s: String, a: java.util.List) => List((s, a.toArray.toList.map{case null => null; case s: String => s; case _ => null}.filter(_ ne null)))
        case _ => Nil
      }
    }
    
    Map(in.entrySet.toArray.toList.flatMap(e => entryToPair(e)) :_*)
  }
}
