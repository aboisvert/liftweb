/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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

import net.liftweb.util.{Can, Full, Empty, Helpers}
import Helpers._
import javax.servlet.http.{HttpServletRequest , HttpServletResponse}
import java.net.{URLConnection}

object ResourceServer {
  private var allowedPaths: PartialFunction[List[String], Boolean] = {
    case "jquery.js" :: Nil => true
    case "json.js" :: Nil => true
    case bp @ ("blueprint" :: _) if bp.last.endsWith(".css") || bp.last.endsWith(".png") => true
    case "jlift.js" :: Nil => true
  }
  
  private var pathRewriter: PartialFunction[List[String], List[String]] = {
     case "jquery.js" :: Nil => List("jquery-1.2.3-min.js")
     case "json.js" :: Nil => List( "json2-min.js")
     case xs => xs
  }
    
  /**
    * The base package for serving resources.  This way, resource names can't be spoofed
    */
  var baseResourceLocation = "toserve"
  
  def findResourceInClasspath(request: RequestState, _uri: List[String])(req: HttpServletRequest): Can[ResponseIt] = {
    val uri = _uri.filter(!_.startsWith("."))
    if (isAllowed(uri)) {
      val rw = baseResourceLocation :: pathRewriter(uri)
      val path = "/"+rw.mkString("/")
      LiftServlet.getResource(path).map{url =>
      val uc = url.openConnection
      val mod = req.getHeader("if-modified-since")
      if (mod != null && ((uc.getLastModified / 1000L) * 1000L) <= parseInternetDate(mod).getTime) Response(new Array[Byte](0), Nil, 304)
      else Response(readWholeStream(url.openStream),
          List(("Last-Modified", toInternetDate(uc.getLastModified)), 
              ("Content-Type", detectContentType(rw.last))),  HttpServletResponse.SC_OK)
      }
    } else Empty
  }
  
  /**
   * detect the Content-Type of file (path) with servlet-context-defined content-types
   * (application's web.xml or servlet container's configuration), and fall
   * back to system or JVM-defined (FileNameMap) content types.
   * if no content-type found, then return "application/octet-stream"
   * 
   * @param path Resource name to be analyzed to detect MIME type
   * 
   * @see ServletContext#getMimeType(String)
   * @see URLConnection#getFileNameMap()
   */
  def detectContentType(path: String) : String = {
    // Configure response with content type of resource
    var contentType = LiftServlet.context.getMimeType(path);
    // If not found, fall back to
    // FileResourceStream.getContentType() that looks into
    // system or JVM content types
    if (contentType == null) {
      contentType = URLConnection.getFileNameMap().getContentTypeFor(path);
    }
    if (contentType == null) {
      contentType = "application/octet-stream"
    }
    contentType
  }
  
  private def isAllowed(path: List[String]) = allowedPaths.isDefinedAt(path) && allowedPaths(path)
   
  def allow(path: PartialFunction[List[String], Boolean]) {
    allowedPaths = path orElse allowedPaths 
  }
  
  def rewrite(rw: PartialFunction[List[String], List[String]]) {
    pathRewriter = rw orElse pathRewriter
  }
}
