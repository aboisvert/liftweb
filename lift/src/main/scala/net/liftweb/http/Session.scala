package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent}
import scala.collection.Map
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.collection.immutable.{TreeMap}
import scala.xml.{NodeSeq, Unparsed, Text}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.collection.immutable.{ListMap}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import java.io.InputStream
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
import net.liftweb.http.S._

class Session extends Actor with HttpSessionBindingListener with HttpSessionActivationListener {
  private val pages = new HashMap[String, Page]
  // private var sessionState: TreeMap[String, Any] = TreeMap.empty
  private var running_? = false
  private var messageCallback: HashMap[String, AFuncHolder] = new HashMap
  private var notices: Seq[(NoticeType.Value, NodeSeq)] = Nil
  private val theControllerMgr = {
    val ret = new ControllerManager
    ret.start
    ret.link(this)
    ret
  }
  
  def sessionDidActivate(se: HttpSessionEvent) = {

  }
  def sessionWillPassivate(se: HttpSessionEvent) = {

  }
  
  /**
   * What happens when this controller is bound to the HTTP session?
   */ 
  def valueBound(event: HttpSessionBindingEvent) {
    //this.start
    // ignore this event  
  }

  /**
   * When the session is unbound the the HTTP controller, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    if (running_?) this ! 'shutdown
  }
  
  /**
   * called when the Actor is started
   */
  def act = {
    this.trapExit = true
    running_? = true
    loop
  }
  
  /**
   * The loop for the actor.  Dispatches messages using Scala's event-based Actors
   */
  final def loop {
    react(dispatcher)
  }
  
  def dispatcher: PartialFunction[Any, Unit] = {
    case 'shutdown =>
      Console.println("Shutting down session")
      theControllerMgr ! 'shutdown
      pages.foreach(_._2 ! 'shutdown)
      self.exit
    loop
    
    case AskSessionToRender(request,httpRequest, timeout) => 
      processRequest(request, httpRequest, timeout)
    loop
    
    case AnswerRenderPage(state, thePage, sender) =>
      val updatedPage = fixResponse(thePage, state)
      sender ! Some(updatedPage)
    loop
    
    case SendEmptyTo(sender) =>
      sender ! XhtmlResponse(Unparsed(""), Map("Content-Type" -> "text/javascript"), 200)
    loop

    case unknown => Console.println("Session Got a message "+unknown); loop
  }
  
  private def processParameters(r: RequestState) {
    r.paramNames.filter(n => messageCallback.contains(n)).foreach{
      n => 
            val f = messageCallback(n)
            f(r.params(n))
    }
  }
  
  private def processRequest(state: RequestState,httpRequest: HttpServletRequest, timeout: long) = {
    S.init(state, httpRequest, notices) {
      try {
        state.testLocation.foreach{s => S.error(s.msg); S.redirectTo(s.to)} 
        
        processParameters(state)
        findVisibleTemplate(state.path, state).map(xml => processSurroundAndInclude(xml, state)) match {
          case None => sender ! state.createNotFound
          case Some(xml: NodeSeq) => {
            S.getFunctionMap.foreach(mi => messageCallback(mi._1) = mi._2)

            if ((xml \\ "controller").filter(_.prefix == "lift").isEmpty) {
              if (state.ajax_?) {
                ActorPing.schedule(this, SendEmptyTo(sender), timeout - 250) 
              } else {
                notices = Nil; sender ! XhtmlResponse(state.fixHtml(xml), Map.empty, 200)
              }
            } else {
              val page = pages.get(state.uri) getOrElse {val p = createPage; pages(state.uri) = p; p }
              page ! AskRenderPage(state, xml, sender, theControllerMgr, timeout)
              if (!state.ajax_?) notices = Nil
            }
          }
        }
      } catch {
        case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[RedirectException]) =>
        val rd = ite.getCause.asInstanceOf[RedirectException]
        notices = S.getNotices
        
        sender ! XhtmlResponse(state.fixHtml(<html><body>{state.uri} Not Found</body></html>),
                 ListMap("Location" -> (state.contextPath+rd.to)),
                 302)
          case rd : net.liftweb.http.RedirectException => {   
            notices = S.getNotices
            
            sender ! XhtmlResponse(state.fixHtml(<html><body>{state.uri} Not Found</body></html>),
                     ListMap("Location" -> (state.contextPath+rd.to)),
                     302)
          }
	case e  => {sender ! state.showException(e)}
      }
    }
  }
  
  /**
   * Create a page based on the uri
   */
  private def createPage: Page = {
        val ret = new Page // FIXME we really want a Page factory here so we can build other Page types
        ret.link(this)
        ret.start
	ret
  }
  
  private def findVisibleTemplate(path: ParsePath, session : RequestState) : Option[NodeSeq] = {
    val tpath = path.path
    val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    findAnyTemplate(splits, session)
  }
  
  private def findTemplate(name: String, session : RequestState) : Option[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    
    findAnyTemplate(splits, session) match {
      case s @ Some(_) => s
      case None => findAnyTemplate("templates-hidden" :: splits, session)
    }
  }
  

  private val suffixes = List("", "html", "xhtml", "htm")
  
  private def findAnyTemplate(places : List[String], session : RequestState) : Option[NodeSeq] = {
    val pls = places.mkString("/","/", "")
    val toTry = suffixes.map(s => pls + (if (s.length > 0) "." + s else ""))
    
    first(toTry)(session.finder(_).flatMap(PCDataXmlParser(_))) orElse lookForClasses(places)
  }  
  
  private def lookForClasses(places : List[String]) : Option[NodeSeq] = {
    val controller: String = (places.take(1) orElse List("default_template"))(0)
    val action: String = (places.drop(1).take(1) orElse List("render"))(0)
    val trans = List((n:String) => n, (n:String) => smartCaps(n))
    val toTry = trans.flatMap(f => (buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "."+f(controller)))

    first(toTry) {
      clsName => 
        try {
          tryo(List(classOf[ClassNotFoundException])) (Class.forName(clsName)).flatMap{
            c =>
              val inst = c.newInstance
              c.getMethod(action, null).invoke(inst, null) match {
                case null | None => None
                case s : NodeSeq => Some(s)
                case Some(n : Seq[Node]) => Some(n)
                case Some(n : NodeSeq) => Some(n)
                case _ => None
              }
            }
        } catch {
          case _ => None
        }
    }
  }
  
  def couldBeHtml(in : Map[String, String]) : boolean = {
    in match {
      case null => true
      case _ => {
	in.get("Content-Type") match {
	  case s @ Some(_) => {s.get.toLowerCase == "text/html"}
	  case None | null => true
	}
      }
    }
  }
  
  def fixResponse(resp: XhtmlResponse, state: RequestState): XhtmlResponse = {
    val newHeaders = fixHeaders(resp.headers, state)
    val newXml = if (couldBeHtml(resp.headers) && state.contextPath.length > 0) state.fixHtml(resp.out) else resp.out
    XhtmlResponse(resp.out, newHeaders, resp.code)
  }
  
  /**
   * Update any "Location" headers to add the Context path
   */
  def fixHeaders(h: Map[String, String], state: RequestState) = {
    h match {
      case null => Map.empty[String, String]
      case _ => Map.empty[String, String] ++ h.map{p =>
        p match {
          case ("Location", v) if (v != null && v.startsWith("/")) => ("Location", "/"+state.contextPath+v)
            case _ => p
        }
						 }
    }
  }
  
  
  private def findAndEmbed(templateName : Option[Seq[Node]], kids : NodeSeq, session : RequestState) : NodeSeq = {
    templateName match {
      case None => Comment("FIX"+"ME No named specified for embedding") ++ kids
      case Some(s) => {
        findTemplate(s.text, session) match {
          case None => Comment("FIX"+"ME Unable to find template named "+s.text) ++ kids
          case Some(s) => processSurroundAndInclude(s, session)
        }
      }
    }
  }
  
  
  private def findControllerByType(contType: Option[Seq[Node]]): Option[ControllerActor] = {
    if (contType == None) None
    else {
      findClass(contType.get.text, buildPackage("controller") ::: ("lift.app.controller" :: Nil), {c : Class => classOf[ControllerActor].isAssignableFrom(c)}) match {
        case None => None
        case Some(cls) => {
          try {
            val ret = Some(cls.newInstance.asInstanceOf[ControllerActor])
            ret
          } catch {
            case _ => None
          }
        }
      }
    }
  }
  
  private def findSnippetClass(name: String): Option[Class] = {
    if (name == null) None
    else findClass(name, buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }
  
  private def findAttributeSnippet(name: String, request: RequestState, rest: MetaData): MetaData = {
    val (cls, method) = splitColonPair(name, null, "render")
    findSnippetClass(cls) match {
      case None => rest
      case Some(clz) => {
        invokeMethod(clz, method) match {
          case Some(md: MetaData) => md.copy(rest)
          case _ => rest
        }
      }
    }
  }

  private def processAttributes(in: MetaData, request: RequestState) : MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
        mine.key match {
          case "snippet" => findAttributeSnippet(mine.value.text, request, processAttributes(in.next, request))
          case _ => processAttributes(in.next, request)
        }
      }
      case notMine => notMine.copy(processAttributes(in.next, request))
    }
  }
  
  private def processSnippet(snippetName: Option[Seq[Node]], attrs: MetaData, kids: NodeSeq, session : RequestState) : NodeSeq = {
    val ret = snippetName match {
      case None => kids
      case Some(ns) => {
          S.locateSnippet(ns.text).map(_(kids)) getOrElse {
            val (cls, method) = splitColonPair(ns.text, null, "render")
        findSnippetClass(cls) match {
	  case None => kids
	  case Some(clz) => {
            ((invokeMethod(clz, method, Array(Group(kids)))) orElse invokeMethod(clz, method)) match {
              case Some(md: NodeSeq) => processSurroundAndInclude(md, session)
              case _ => kids
            }
	  }
        }
        }
      }
    }
    
    attrs.get("form").map(ft => <form action={S.request.uri} method={ft.text}>{ret}</form>) getOrElse ret
  }
  
  private def processSurroundAndInclude(in : NodeSeq, session : RequestState) : NodeSeq = {
    in.flatMap{
      v => 
        v match {
          case Elem("lift", "ignore", attr @ _, _, kids @ _*) => Text("")
          case Elem("lift", "surround", attr @ _, _, kids @ _*) => findAndMerge(attr.get("with"), attr.get("at"), processSurroundAndInclude(kids, session), session)
          case Elem("lift", "embed", attr @ _, _, kids @ _*) => findAndEmbed(attr.get("what"), processSurroundAndInclude(kids, session), session)
          case Elem("lift", "snippet", attr @ _, _, kids @ _*) if (!session.ajax_? || toBoolean(attr("ajax"))) => S.setVars(attr)(processSnippet(attr.get("type"), attr, processSurroundAndInclude(kids, session), session))
          case Elem("lift", "children", attr @ _, _, kids @ _*) => processSurroundAndInclude(kids, session)
          case Elem("lift", "vars", attr @ _, _, kids @ _*) => S.setVars(attr)(processSurroundAndInclude(kids, session))
          case Elem(_,_,_,_,_*) => Elem(v.prefix, v.label, processAttributes(v.attributes, session), v.scope, processSurroundAndInclude(v.child, session) : _*)
          case _ => {v}
        }
    }
  }
  
  def findAndMerge(templateName : Option[Seq[Node]], at : Option[Seq[Node]], kids : NodeSeq, session : RequestState) : NodeSeq = {
    val name : String = templateName match {
      case None => "/templates-hidden/Default"
      case Some(s) => if (s.text.startsWith("/")) s.text else "/"+ s.text
    }
    
    findTemplate(name, session) match {
      case None => kids
      case Some(s) => processBind(processSurroundAndInclude(s, session), at.map(_.text) getOrElse "main", kids)
    }
  }
}

abstract class SessionMessage

//case class Setup(page: NodeSeq, http: Actor) extends SessionMessage
//case class SetGlobal(name: String, value: Any) extends SessionMessage
//case class UnsetGlobal(name: String) extends SessionMessage
/**
 * Sent from a session to a Page to tell the page to render itself and includes the sender that
 * the rendered response should be sent to
 */
case class AskRenderPage(state: RequestState, xml: NodeSeq, sender: Actor, controllerMgr: ControllerManager, timeout: long) extends SessionMessage

/**
 * The response from a page saying that it's been rendered
 */
case class AnswerRenderPage(state: RequestState, thePage: XhtmlResponse, sender: Actor) extends SessionMessage
case class AskSessionToRender(request: RequestState,httpRequest: HttpServletRequest,timeout: long)
case class SendEmptyTo(who:Actor) extends SessionMessage



