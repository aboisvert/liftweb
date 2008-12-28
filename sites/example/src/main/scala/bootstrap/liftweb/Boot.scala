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

import _root_.net.liftweb._
import util.{Helpers, Box, Full, Empty, Failure, Log, NamedPF}
import http._
import sitemap._
import Helpers._

import example._
import comet.WebServices
import model._
import lib._
import snippet.{definedLocale, Template}

import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}

import _root_.java.sql.{Connection, DriverManager}
import _root_.javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import _root_.scala.actors._
import Actor._

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

    LiftRules.dispatch.prepend(NamedPF("Web Services Example") {
        // if the url is "showcities" then return the showCities function
        case Req("showcities":: _, "", _) => XmlServer.showCities

          // if the url is "showstates" "curry" the showStates function with the optional second parameter
        case Req("showstates":: xs, "", _) =>
          XmlServer.showStates(if (xs.isEmpty) "default" else xs.head)

          // if it's a web service, pass it to the web services invoker
        case Req("webservices" :: c :: _, "", _) => invokeWebService(c)
      })

    LiftRules.dispatch.prepend(NamedPF("Login Validation") {
        case Req("login" :: page , "", _)
          if !LoginStuff.is && page.head != "validate" =>
          () => Full(RedirectResponse("/login/validate"))
      })

    LiftRules.snippetDispatch.append(NamedPF("Template")(Map("Template" -> Template)))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
    Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
    Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    val wikibind_rewriter: LiftRules.RewritePF = NamedPF("WikiBind") {
      case RewriteRequest(path @ ParsePath("wikibind" :: page :: _, _, _,_),
                          _, _)
        =>
        RewriteResponse(ParsePath("wikibind" :: Nil, "", true, false),
                        Map("wiki_page" -> page ::
                            path.wholePath.drop(2).zipWithIndex.
                            map(p => ("param"+(p._2 + 1)) -> p._1) :_*))
    }

    LiftRules.early.append(makeUtf8)

    LiftRules.rewrite.prepend(wikibind_rewriter)

    LiftSession.onBeginServicing = RequestLogger.beginServicing _ ::
    LiftSession.onBeginServicing

    LiftSession.onEndServicing = RequestLogger.endServicing _ ::
    LiftSession.onEndServicing

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

    // Dump information about session every 10 minutes
    SessionMaster.sessionWatchers = SessionInfoDumper :: SessionMaster.sessionWatchers

    // Dump browser information each time a new connection is made
    LiftSession.onBeginServicing = BrowserLogger.haveSeenYou _ :: LiftSession.onBeginServicing

  }

  private def invokeWebService(methodName: String)():
  Box[LiftResponse] =
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

  def beginServicing(session: LiftSession, req: Req) {
    startTime(millis)
  }

  def endServicing(session: LiftSession, req: Req,
                   response: Box[LiftResponse]) {
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
  Menu(Loc("template", List("template"), "Templates")) ::
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
  // Menu(Loc("wiki", Link(List("wiki"), true, "/wiki/HomePage"), "Wiki")) ::
  Menu(WikiStuff) ::
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
  def showStates(which: String)(): Box[XmlResponse] = Full(XmlResponse(
      <states renderedAt={timeNow.toString}>{
          which match {
            case "red" => <state name="Ohio"/><state name="Texas"/><state name="Colorado"/>

            case "blue" => <state name="New York"/><state name="Pennsylvania"/><state name="Vermont"/>

            case _ => <state name="California"/><state name="Rhode Island"/><state name="Maine"/>
          } }
      </states>))

  def showCities(): Box[XmlResponse] =
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
  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
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

object BrowserLogger {
  object HaveSeenYou extends SessionVar(false)

  def haveSeenYou(session: LiftSession, request: Req) {
    if (!HaveSeenYou.is) {
      Log.info("Created session "+session.uniqueId+" IP: {"+request.request.getRemoteAddr+"} UserAgent: {{"+request.userAgent.openOr("N/A")+"}}")
      HaveSeenYou(true)
    }
  }
}

object SessionInfoDumper extends Actor {
  private var lastTime = millis

  val tenMinutes: Long = 10 minutes
  def act = {
    link(ActorWatcher)
    loop {
      react {
        case SessionWatcherInfo(sessions) =>
          if ((millis - tenMinutes) > lastTime) {
            lastTime = millis
            val rt = Runtime.getRuntime
            Log.info("Number of open sessions: "+sessions.size)
            Log.info("Free Memory: "+rt.freeMemory)
            Log.info("Total Memory: "+rt.totalMemory)
          }
      }
    }
  }

  this.start
}
