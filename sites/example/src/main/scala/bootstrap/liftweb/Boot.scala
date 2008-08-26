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
package bootstrap.liftweb

import net.liftweb.util.{Helpers, Can, Full, Empty, Failure, Log}
import net.liftweb.http._
import net.liftweb._
import sitemap._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import net.liftweb.example.comet.WebServices
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import net.liftweb.example._
import model._
import lib._
import net.liftweb.example.snippet.definedLocale

/**
* A class that's instantiated early and run.  It allows the application
* to modify lift's environment
*/
class Boot {
  def boot {
    DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    LiftRules.addToPackages("net.liftweb.example")

    LiftRules.localeCalculator = r => definedLocale.openOr(LiftRules.defaultLocaleCalculator(r))

    Schemifier.schemify(true, Log.infoF _, User, WikiEntry, Person)

    LiftRules.addDispatchBefore {
      // if the url is "showcities" then return the showCities function
      case RequestState("showcities":: _, "", _) => XmlServer.showCities

      // if the url is "showstates" "curry" the showStates function with the optional second parameter
      case RequestState("showstates":: xs, "", _) => 
	XmlServer.showStates(if (xs.isEmpty) "default" else xs.head)

      // if it's a web service, pass it to the web services invoker
      case RequestState("webservices" :: c :: _, "", _) => invokeWebService(c)
    }

    LiftRules.addDispatchBefore {
         case RequestState("login" :: page , "", _) 
      if !LoginStuff.is && page.head != "validate" =>
        () => Full(RedirectResponse("/login/validate"))
    }


    LiftRules.addRewriteBefore{
      case RewriteRequest( path @ ParsePath("wiki" :: page :: _, _, _,_),
			  _, _)
      =>
	RewriteResponse("wiki" :: Nil,
			Map("wiki_page" -> page ::
			    path.wholePath.drop(2).
			    zipWithIndex.map(p =>
			      ("param"+(p._2 + 1)) -> p._1) :_*))
    }

    val wikibind_rewriter: LiftRules.RewritePf = {
      case RewriteRequest(path @ ParsePath("wikibind" :: page :: _, _, _,_),
			  _, _)
      =>
	RewriteResponse(ParsePath("wikibind" :: Nil, "", true, false),
			Map("wiki_page" -> page ::
			    path.wholePath.drop(2).zipWithIndex.
			    map(p => ("param"+(p._2 + 1)) -> p._1) :_*))
    }

    LiftRules.appendEarly(makeUtf8)

    LiftRules.addRewriteBefore(wikibind_rewriter)

    LiftSession.onBeginServicing = RequestLogger.beginServicing _ ::
    LiftSession.onBeginServicing

    LiftSession.onEndServicing = RequestLogger.endServicing _ ::
    LiftSession.onEndServicing

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

  }

  private def invokeWebService(methodName: String)():
  Can[LiftResponse] =
    for (req <- S.request;
	 invoker <- createInvoker(methodName, new WebServices(req));
	 ret <- invoker() match {
	   case Full(ret: LiftResponse) => Full(ret)
	   case _ => Empty
	 }) yield ret
  
  private def makeUtf8(req: HttpServletRequest): Unit = {req.setCharacterEncoding("UTF-8")}
}

object RequestLogger {
  object startTime extends RequestVar(0L)

  def beginServicing(session: LiftSession, req: RequestState) {
    startTime(millis)
  }

  def endServicing(session: LiftSession, req: RequestState,
                   response: Can[LiftResponse]) {
    val delta = millis - startTime.is
    Log.info("Serviced "+req.uri+" in "+(delta)+"ms "+(
        response.map(r => " Headers: "+r.toResponse.headers) openOr ""
      ))
  }
}

object MenuInfo {
  import Loc._

  def menu: List[Menu] =  Menu(Loc("home", List("index"), "Home")) ::
  Menu(Loc("xml fun", List("xml_fun"), "XML Fun")) ::
  Menu(Loc("chat", List("chat"), "Comet Chat")) ::
  Menu(Loc("database", List("database"), "Database")) ::
  Menu(Loc("ajax", List("ajax"), "AJAX Samples")) ::
  Menu(Loc("ajax form", List("ajax-form"), "AJAX Form")) ::
  Menu(Loc("json", List("json"), "JSON Messaging")) ::
  Menu(Loc("ws", List("ws"), "Web Services")) ::
  Menu(Loc("simple", Link(List("simple"), true, "/simple/index"),
	   "Simple Forms")) ::
  Menu(Loc("lang", List("lang"), "Localization")) ::
  Menu(Loc("menu_top", List("menu", "index"), "Menus"),
       Menu(Loc("menu_one", List("menu", "one"), "First Submenu")),
       Menu(Loc("menu_two", List("menu", "two"), "Second Submenu (has more)"),
	    Menu(Loc("menu_two_one", List("menu", "two_one"), 
		     "First (2) Submenu")),
	    Menu(Loc("menu_two_two", List("menu", "two_two"), 
		     "Second (2) Submenu"))
	  ),
       Menu(Loc("menu_three", List("menu", "three"), "Third Submenu")),
       Menu(Loc("menu_four", List("menu", "four"), "Forth Submenu"))
     ) ::
  Menu(Loc("file_upload", List("file_upload"), "File Upload")) ::
  Menu(Loc("wiki", Link(List("wiki"), true, "/wiki/HomePage"), "Wiki")) ::
  Menu(Loc("guess", List("guess"), "Number Guessing")) ::
  Menu(Loc("count", List("count"), "Counting")) ::
  Menu(Loc("login", Link(List("login"), true, "/login/index"), 
	   <xml:group>Requiring Login <strike>SiteMap</strike></xml:group>)) ::
  Menu(Loc("arc", List("arc"), "Arc Challenge #1")) ::
  Menu(Loc("lift", ExtLink("http://liftweb.net"), 
	 <xml:group><i>Lift</i> project home</xml:group>)) ::
  Nil
}

object XmlServer {
  def showStates(which: String)(): Can[XmlResponse] = Full(XmlResponse(
    <states renderedAt={timeNow.toString}>{
      which match {
	case "red" => <state name="Ohio"/><state name="Texas"/><state name="Colorado"/>
	
	case "blue" => <state name="New York"/><state name="Pennsylvania"/><state name="Vermont"/>
	
	case _ => <state name="California"/><state name="Rhode Island"/><state name="Maine"/>
      } }
    </states>))
  
  def showCities(): Can[XmlResponse] = 
    Full(XmlResponse(
      <cities>
      <city name="Boston"/>
      <city name="New York"/>
      <city name="San Francisco"/>
      <city name="Dallas"/>
      <city name="Chicago"/>
      </cities>))
  
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.h2.Driver")
      val dm =  DriverManager.getConnection("jdbc:h2:mem:lift;DB_CLOSE_DELAY=-1")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}
