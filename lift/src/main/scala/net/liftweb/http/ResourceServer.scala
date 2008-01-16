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

import net.liftweb.util.{Can, Full, Empty}
import javax.servlet.http.{HttpServletRequest , HttpServletResponse}
import java.net.{URLConnection}

object ResourceServer {
  def findResourceInClasspath(request: RequestState, uri: List[String])(req: HttpServletRequest): Can[ResponseIt] = {
    val uriPath = uri.foldLeft("")(_ + "/" + _);
    val header = List(("Content-Type", detectContentType(uri.last)))
    LiftServlet.loadResource(uriPath) match {
      case Full(data :Array[Byte]) => Full(Response(data, header, HttpServletResponse.SC_OK))
      case _ => Empty
    }
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
}