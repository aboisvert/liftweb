package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{Node, Unparsed}
import net.liftweb.util._
import net.liftweb.util.Helpers._

trait ToResponse extends ResponseIt {
  def out: Node
  def headers: List[(String, String)]
  def code: Int
  def docType: Can[String]
  
  def toResponse = {
    val encoding = 
    (out, headers.ciGet("Content-Type")) match {
    case (up: Unparsed,  _) => ""
    case (_, Empty) | (_, Failure(_, _, _)) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case (_, Full(s)) if (s.toLowerCase.startsWith("text/xml") ||
                          s.toLowerCase.startsWith("text/html") ||
                          s.toLowerCase.startsWith("text/xhtml") ||
			  s.toLowerCase.startsWith("application/xml") ||
			  s.toLowerCase.startsWith("application/xhtml+xml")) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case _ => ""
    }
    
    val doc = docType.map(_ + "\n") openOr ""
      
    Response((encoding + doc + AltXML.toXML(out, false, false)).getBytes("UTF-8"), headers, code)
    }
}

trait ResponseIt {
  def toResponse: Response
}

case class XhtmlResponse(out: Node, docType: Can[String], headers: List[(String, String)], code: Int) extends ToResponse

case class Response(data: Array[Byte], headers: List[(String, String)], code: Int) extends ResponseIt {
  def toResponse = this
}

object ResponseInfo {
  var xhtmlTransitional: PartialFunction[RequestState, Can[String]] = {
    case _ => Full("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
  }
}
