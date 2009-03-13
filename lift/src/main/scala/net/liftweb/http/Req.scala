/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.http

import _root_.javax.servlet.http._
import _root_.javax.servlet.ServletContext
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.{Log, Box, Full, Empty,
                                EmptyBox,
                                Failure, ThreadGlobal,
                                NamedPF, NamedPartialFunction}
import _root_.net.liftweb.sitemap._
import _root_.java.io.InputStream
import _root_.scala.xml._
import _root_.org.apache.commons.fileupload.servlet._

@serializable
sealed trait ParamHolder {
  def name: String
}
@serializable
case class NormalParamHolder(name: String, value: String) extends ParamHolder
@serializable
case class FileParamHolder(name: String, mimeType: String,
                           fileName: String,
                           file: Array[Byte]) extends ParamHolder

/**
 * Helper object for constructing Req instances
 */
object Req {
  object NilPath extends ParsePath(Nil, "", true, false)

  def apply(request: HttpServletRequest, rewrite: List[LiftRules.RewritePF], nanoStart: Long): Req = {
    val reqType = RequestType(request)
    val turi = request.getRequestURI.substring(request.getContextPath.length)
    val tmpUri = if (turi.length > 0) turi else "/"
    val contextPath = LiftRules.calculateContextPath(request) openOr
    request.getContextPath

    val tmpPath = parsePath(tmpUri)

    def processRewrite(path: ParsePath, params: Map[String, String]): RewriteResponse =
    NamedPF.applyBox(RewriteRequest(path, reqType, request), rewrite) match {
      case Full(resp @ RewriteResponse(_, _, true)) => resp
      case _: EmptyBox[_] => RewriteResponse(path, params)
      case Full(resp) => processRewrite(resp.path, resp.params)
    }



    // val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    val rewritten = processRewrite(tmpPath, Map.empty)

    val localParams: Map[String, List[String]] = Map(rewritten.params.toList.map{case (name, value) => name -> List(value)} :_*)

    // val session = request.getSession
    //  val body = ()
    val eMap = Map.empty[String, List[String]]

  
    val contentType = request.getContentType match {
      case null => ""
      case s => s
    }

    //    val (paramNames: List[String], params: Map[String, List[String]], files: List[FileParamHolder], body: Box[Array[Byte]]) =
    val paramCalculator = () =>
    if ((reqType.post_? ||
         reqType.put_?) && contentType.startsWith("text/xml")) {
      (Nil,localParams, Nil, tryo(readWholeStream(request.getInputStream)))
    } else if (ServletFileUpload.isMultipartContent(request)) {
      val allInfo = (new Iterator[ParamHolder] {
          val mimeUpload = (new ServletFileUpload)
          mimeUpload.setSizeMax(LiftRules.maxMimeSize)
          mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
          val what = mimeUpload.getItemIterator(request)
          def hasNext = what.hasNext
          def next = what.next match {
            case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.openStream), "UTF-8"))
            case f => FileParamHolder(f.getFieldName, f.getContentType, f.getName, readWholeStream(f.openStream))
          }
        }).toList

      val normal: List[NormalParamHolder] = allInfo.flatMap{case v: NormalParamHolder => List(v) case _ => Nil}
      val files: List[FileParamHolder] = allInfo.flatMap{case v: FileParamHolder => List(v) case _ => Nil}

      val params = normal.foldLeft(eMap)((a,b) => a.get(b.name) match {
          case None => a + (b.name -> List(b.value))
          case Some(v) => a + (b.name -> (v ::: List(b.value)))
        })

      (normal.map(_.name).removeDuplicates, localParams ++ params, files, Empty)
    } else if (reqType.get_?) {
      request.getQueryString match {
        case null => (Nil, localParams, Nil, Empty)
        case s =>
          val pairs = s.split("&").toList.map(_.trim).filter(_.length > 0).map(_.split("=").toList match {
              case name :: value :: Nil => (true, urlDecode(name), urlDecode(value))
              case name :: Nil => (true, urlDecode(name), "")
              case _ => (false, "", "")
            }).filter(_._1).map{case (_, name, value) => (name, value)}
          val names = pairs.map(_._1).removeDuplicates
          val params = pairs.foldLeft(eMap) (
            (a,b) => a.get(b._1) match {
              case None => a + (b._1 -> List(b._2))
              case Some(xs) => a + (b._1 -> (xs ::: List(b._2)))
            }
          )

          val hereParams = localParams ++ params

          (names, hereParams, Nil, Empty)
      }
    } else if (contentType.toLowerCase.startsWith("application/x-www-form-urlencoded")) {
      val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
      // val tmp = paramNames.map{n => (n, xlateIfGet(request.getParameterValues(n).toList))}
      val params = localParams ++ paramNames.map{n => (n, request.getParameterValues(n).toList)}
      (paramNames, params, Nil, Empty)
    } else {
      (Nil,localParams, Nil, tryo(readWholeStream(request.getInputStream)))
    }

    new Req(rewritten.path, contextPath, reqType,
            contentType, request, nanoStart,
            System.nanoTime, paramCalculator)
  }

  private def fixURI(uri : String) = uri indexOf ";jsessionid"  match {
    case -1 => uri
    case x @ _ => uri substring(0, x)
  }

  def nil = new Req(NilPath, "", GetRequest, "", null,
                    System.nanoTime, System.nanoTime,
                    () => (Nil, Map.empty, Nil, Empty))

  def parsePath(in: String): ParsePath = {
    val p1 = fixURI((in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/"))
    val front = p1.startsWith("/")
    val back = p1.length > 1 && p1.endsWith("/")

    val orgLst = p1.replaceAll("/$", "/index").split("/").
    toList.map(_.trim).filter(_.length > 0)

    val last = orgLst.last
    val idx = last.indexOf(".")

    val (lst, suffix) = if (idx == -1) (orgLst, "")
    else (orgLst.dropRight(1) ::: List(last.substring(0, idx)),
          last.substring(idx + 1))

    ParsePath(lst.map(urlDecode), suffix, front, back)
  }

  var fixHref = _fixHref _

  private def _fixHref(contextPath: String, v : Seq[Node], fixURL: Boolean, rewrite: Box[String => String]): Text = {
    val hv = v.text
    val updated = if (hv.startsWith("/")) contextPath + hv else hv

    Text(if (fixURL && rewrite.isDefined &&
             !updated.startsWith("javascript:") &&
             !updated.startsWith("http://") &&
             !updated.startsWith("https://"))
         rewrite.open_!.apply(updated) else updated)
  }

  /**
   * Corrects the HTML content,such as applying context path to URI's, session information if cookies are disabled etc.
   */
  def fixHtml(contextPath: String, in: NodeSeq): NodeSeq = {
    val rewrite = URLRewriter.rewriteFunc

    def fixAttrs(toFix : String, attrs : MetaData, fixURL: Boolean) : MetaData = {
      if (attrs == Null) Null
      else if (attrs.key == toFix) {
        new UnprefixedAttribute(toFix, Req.fixHref(contextPath, attrs.value, fixURL, rewrite),fixAttrs(toFix, attrs.next, fixURL))
      } else attrs.copy(fixAttrs(toFix, attrs.next, fixURL))
    }

    def _fixHtml(contextPath: String, in: NodeSeq): NodeSeq = {
      in.map{
        v =>
        v match {
          case Group(nodes) => Group(_fixHtml(contextPath, nodes))
          case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs("action", v.attributes, true), v.scope, _fixHtml(contextPath, v.child) : _* )
          case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, false), v.scope, _fixHtml(contextPath, v.child) : _* )
          case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, true), v.scope, _fixHtml(contextPath, v.child) : _* )
          case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, false), v.scope, _fixHtml(contextPath, v.child) : _* )
          case e: Elem => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, true), v.scope, _fixHtml(contextPath, v.child) : _*)
          case _ => v
        }
      }
    }
    _fixHtml(contextPath, in)
  }

  private[liftweb] def defaultCreateNotFound(in: Req) =
  XhtmlResponse((<html><body>The Requested URL {in.contextPath+in.uri} was not found on this server</body></html>),
                ResponseInfo.docType(in), List("Content-Type" -> "text/html"), Nil, 404, S.ieMode)

  def unapply(in: Req): Option[(List[String], String, RequestType)] = Some((in.path.partPath, in.path.suffix, in.requestType))
}

/**
 * Contains request information
 */
@serializable
class Req(val path: ParsePath,
          val contextPath: String,
          val requestType: RequestType,
          val contentType: String,
          val request: HttpServletRequest,
          val nanoStart: Long,
          val nanoEnd: Long,
          val paramCalculator: () => (List[String], Map[String, List[String]],List[FileParamHolder],Box[Array[Byte]])) extends HasParams
{

  override def toString = "Req("+paramNames+", "+params+", "+path+
  ", "+contextPath+", "+requestType+", "+contentType+")"

  /**
   * Returns true if the content-type is text/xml
   */
  def xml_? = contentType != null && contentType.toLowerCase.startsWith("text/xml")

  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}

  val id = pathParam(0)
  def pathParam(n: Int) = head(path.wholePath.drop(n + 2), "")
  def path(n: Int):String = head(path.wholePath.drop(n), null)
  def param(n: String) = params.get(n) match {
    case Some(s :: _) => Some(s)
    case _ => None
  }

  lazy val headers: List[(String, String)] =
  for (header <- enumToList[String](request.getHeaderNames.asInstanceOf[_root_.java.util.Enumeration[String]]);
       item <- enumToList[String](request.getHeaders(header).asInstanceOf[_root_.java.util.Enumeration[String]]))
  yield (header, item)

  lazy val (paramNames: List[String],
            params: Map[String, List[String]],
            uploadedFiles: List[FileParamHolder],
            body: Box[Array[Byte]]) = paramCalculator()

  lazy val cookies = request.getCookies() match {
    case null => Nil
    case ca => ca.toList
  }

  lazy val xml: Box[Elem] = if (!xml_?) Empty
  else {
    try {
      body.map(b => XML.load(new _root_.java.io.ByteArrayInputStream(b)))
    } catch {
      case e => Failure(e.getMessage, Full(e), Empty)
    }
  }

  lazy val location: Box[Loc[_]] = LiftRules.siteMap.flatMap(_.findLoc(this))

  def testLocation: Either[Boolean, Box[LiftResponse]] = {
    if (LiftRules.siteMap.isEmpty) Left(true)
    else location.map(_.testAccess) match {
      case Full(Left(true)) => Left(true)
      case Full(Right(Full(resp))) => Right(Full(resp))
      case _ => Right(Empty)
    }
  }


  lazy val buildMenu: CompleteMenu = location.map(_.buildMenu) openOr
  CompleteMenu(Nil)


  def createNotFound = {
    NamedPF((this, Empty), LiftRules.uriNotFound.toList)
  }

  def createNotFound(failure: Failure) = {
    NamedPF((this, Full(failure)), LiftRules.uriNotFound.toList)
  }


  def post_? = requestType.post_?
  def get_? = requestType.get_?
  def put_? = requestType.put_?

  def fixHtml(in: NodeSeq): NodeSeq = Req.fixHtml(contextPath, in)

  lazy val uri: String = request match {
    case null => "Outside HTTP Request (e.g., on Actor)"
    case request =>
      val ret = for (uri <- Box.legacyNullTest(request.getRequestURI);
                     val cp = Box.legacyNullTest(request.getContextPath) openOr "") yield
      uri.substring(cp.length)
      match {
        case "" => "/"
        case x => Req.fixURI(x)
      }
      ret openOr "/"
  }

  /**
   * The IP address of the request
   */
  def remoteAddr: String = request.getRemoteAddr()

  /**
   * The user agent of the browser that sent the request
   */
  lazy val userAgent: Box[String] =
  for (r <- Box.legacyNullTest(request);
       uah <- Box.legacyNullTest(request.getHeader("User-Agent")))
  yield uah

  lazy val isIE6: Boolean = (userAgent.map(_.indexOf("MSIE 6") >= 0)) openOr false
  lazy val isIE7: Boolean = (userAgent.map(_.indexOf("MSIE 7") >= 0)) openOr false
  lazy val isIE8: Boolean = (userAgent.map(_.indexOf("MSIE 8") >= 0)) openOr false
  lazy val isIE = isIE6 || isIE7 || isIE8

  lazy val isSafari2: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/2.") >= 0)) openOr false

  lazy val isSafari3: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/3.") >= 0)) openOr false
  lazy val isSafari = isSafari2 || isSafari3

  lazy val isIPhone = isSafari && (userAgent.map(s => s.indexOf("(iPhone;") >= 0) openOr false)

  lazy val isFirefox2: Boolean = (userAgent.map(_.indexOf("Firefox/2") >= 0)) openOr false
  lazy val isFirefox3: Boolean = (userAgent.map(_.indexOf("Firefox/3") >= 0)) openOr false
  lazy val isFirefox = isFirefox2 || isFirefox3

  lazy val isOpera9: Boolean = (userAgent.map(s => s.indexOf("Opera/9.") >= 0) openOr false)
  def isOpera = isOpera9


  def updateWithContextPath(uri: String): String = if (uri.startsWith("/")) contextPath + uri else uri
}

case class RewriteRequest(path: ParsePath, requestType: RequestType, httpRequest: HttpServletRequest)
case class RewriteResponse(path: ParsePath, params: Map[String, String], stopRewriting: Boolean)

/**
 * The representation of an URI path
 */
@serializable
case class ParsePath(partPath: List[String], suffix: String, absolute: Boolean, endSlash: Boolean) {
  def drop(cnt: Int) = ParsePath(partPath.drop(cnt), suffix, absolute, endSlash)

  lazy val wholePath = if (suffix.length > 0) partPath.dropRight(1) ::: List(partPath.last + "." + suffix)
  else partPath
}

/**
 * Maintains the context of resolving the URL when cookies are disabled from container. It maintains
 * low coupling such as code within request processing is not aware of the servlet response that
 * ancodes the URL.
 */
object RewriteResponse {
  def apply(path: List[String], params: Map[String, String]) = new RewriteResponse(ParsePath(path, "", true, false), params, false)
  def apply(path: List[String]) = new RewriteResponse(ParsePath(path, "", true, false), Map.empty, false)

  def apply(path: List[String], suffix: String) = new RewriteResponse(ParsePath(path, suffix, true, false), Map.empty, false)

  def apply(path: ParsePath, params: Map[String, String]) = new RewriteResponse(path, params, false)
}

object URLRewriter {
  private val funcHolder = new ThreadGlobal[(String) => String]

  def doWith[R](f: (String) => String)(block : => R):R = {
    funcHolder.doWith(f) {
      block
    }
  }

  def rewriteFunc: Box[(String) => String] = Box.legacyNullTest(funcHolder value)
}
