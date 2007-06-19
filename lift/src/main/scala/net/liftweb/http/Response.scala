package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{Node, Unparsed}
import net.liftweb.util._
import net.liftweb.util.Helpers._

trait ToResponse {
  def out: Node
  def headers: List[(String, String)]
  def code: Int
  def docType: Option[String]
  
  def toResponse = {
    val encoding = 
    (out, headers.ciGet("Content-Type")) match {
    case (up: Unparsed,  _) => ""
    case (_, None) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case (_, Some(s)) if (s.toLowerCase.startsWith("text/xml") ||
                          s.toLowerCase.startsWith("text/html") ||
                          s.toLowerCase.startsWith("text/xhtml")) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case _ => ""
    }
    
    val doc = docType.map(_ + "\n") getOrElse ""
      
    Response((encoding + doc + AltXML.toXML(out, false)).getBytes("UTF-8"), headers, code)
    }
// "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    // "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
}

case class XhtmlResponse(out: Node, docType: Option[String], headers: List[(String, String)], code: Int) extends ToResponse 

case class Response(data: Array[byte], headers: List[(String, String)], code: Int)

object ResponseInfo {
  val xhtmlTransitional = Some("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
}