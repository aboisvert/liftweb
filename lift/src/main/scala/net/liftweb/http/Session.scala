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
import scala.collection.mutable.{HashMap}
import scala.collection.immutable.{TreeMap}
import scala.xml.{NodeSeq}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.collection.immutable.{ListMap}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment}
import java.io.InputStream
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent}


class Session extends Actor with HttpSessionBindingListener with HttpSessionActivationListener {
  private val pages = new HashMap[String, Page]
  private var sessionState: TreeMap[String, Any] = TreeMap.empty
  private var running_? = false
  
  def sessionDidActivate(se: HttpSessionEvent) = {
    // Console.println("session Did activate")
  }
  def sessionWillPassivate(se: HttpSessionEvent) = {
    /*
     val session = se.getSession
     val atNames = session.getAttributeNames
     while (atNames.hasMoreElements) {
     atNames.nextElement match {
     
     case s: String => Console.println("Removed "+s); session.removeAttribute(s)
     case o => Console.println("Didn't remove "+o)
     }
     }
    Console.println("session Did passivate real good!")
    */
  }
  /**
  * What happens when this controller is bound to the HTTP session?
  */ 
  def valueBound(event: HttpSessionBindingEvent) {
    //Console.println("bound ")
    //this.start
    // ignore this event  
  }

  /**
  * When the session is unbound the the HTTP controller, stop us
  */
  def valueUnbound(event: HttpSessionBindingEvent) {
    if (running_?) this ! "shutdown"
  }
  
  /**
  * called when the Actor is started
  */
  def act = {
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
    case "shutdown" => self.exit("shutdown")
    loop
    
    case AskSessionToRender(request,finder,timeout) => 
      processRequest(request, finder, timeout)
    loop
    
    case AnswerRenderPage(state: RequestState, thePage: Response, sender: Actor) =>
      val updatedPage = fixResponse(thePage, state)
    sender ! Some(updatedPage)
    loop

    case SetGlobal(name, value) => sessionState = (sessionState(name) = value); loop
    case UnsetGlobal(name) => sessionState = (sessionState - name); loop
    case _ => loop
  }
  
  private def processRequest(state: RequestState, finder: (String) => InputStream, timeout: long) = {
    S.init(state) {
      try {
	val page = 
	  pages.get(state.uri) match {
	    
	    case None => {
              createPage(state, finder) match {
		case None => None
		case s @ Some(p: Page) =>
		  pages(state.uri) = p
		s
              }
	    }
	    case Some(p : Page) => {
              findVisibleTemplate(state.uri, state, finder) match {
		case Some(xml: NodeSeq) => p ! PerformSetupPage(processSurroundAndInclude(xml, state, finder), this) // FIXME reloads the page... maybe we don't always want to do this
		case _ => {}
              }
              Some(p)
	    }
	  }
	
	page match {
	  case Some(p) =>  p.forward(AskRenderPage(state, sessionState, sender, controllerMgr, timeout))
	  case _ => reply(state.createNotFound)
	}
      } catch {
	case e  => {reply(state.showException(e))}
      }
    }
  }
  
  private val controllerMgr = {
    val ret = new ControllerManager
    ret.start
    ret.link(self)
    ret
  }

  /**
  * Create a page based on the uri
  */
  private def createPage(state: RequestState, finder: (String) => InputStream): Option[Page] = {
    findVisibleTemplate(state.uri, state, finder) match {
      case None => None
      case Some(xml: NodeSeq) => {
        val ret = new Page // FIXME we really want a Page factory here so we can build other Page types
        ret.link(self)
        ret.start
        val realXml : NodeSeq = processSurroundAndInclude(xml, state, finder)
        ret ! PerformSetupPage(realXml, this)
        Some(ret)
      }
    }
  }
  
  private def findVisibleTemplate(name : String, session : RequestState, finder: (String) => InputStream) : Option[NodeSeq] = {
    val splits = name.split("/").toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1}.drop(1) match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    findAnyTemplate(splits, session, finder)
  }
  
  private def findTemplate(name : String, session : RequestState, finder: (String) => InputStream) : Option[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    
    findAnyTemplate(splits, session, finder) match {
      case s @ Some(_) => s
      case None => findAnyTemplate("templates-hidden" :: splits, session, finder)
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
  
  def fixResponse(resp: Response, state: RequestState): Response = {
    val newHeaders = fixHeaders(resp.headers, state)
    val newXml = if (couldBeHtml(resp.headers) && state.contextPath.length > 0) state.fixHtml(resp.out) else resp.out
    Response(resp.out, newHeaders, resp.code)
  }
  
  /**
  * Update any "Location" headers to add the Context path
  */
  def fixHeaders(h: Map[String, String], state: RequestState) = {
    h match {
      case null => Map.empty[String, String]
      case _ => Map.empty[String, String] ++ h.map{p =>
        p match {
          case {"Location", v} if (v != null && v.startsWith("/")) => {"Location", "/"+state.contextPath+v}
          case _ => p
        }
						 }
    }
  }
  

  /*
  private def processTemplate(xml : NodeSeq, session: RequestState) : Option[Response] = {
    val surrounded = processSurroundAndInclude(xml, session)
  val withController = processControllers(surrounded)
  
  Some(Response(withController, ListMap.Empty, 200))
  }*/
  
  private def findAndImbed(templateName : Option[Seq[Node]], kids : NodeSeq, session : RequestState, finder: (String) => InputStream) : NodeSeq = {
    templateName match {
      case None => Comment("FIX"+"ME No named specified for embedding") concat kids
      case Some(s) => {
        findTemplate(s.text, session, finder) match {
          case None => Comment("FIX"+"ME Unable to find template named "+s.text) concat kids
          case Some(s) => processSurroundAndInclude(s, session, finder)
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
    else findClass(name, buildPackage("snippet") ::: ("net.liftweb.builtin.snippet" :: Nil))
  }
  
  private def findAttributeSnippet(name: String, request: RequestState, finder: (String) => InputStream, rest: MetaData): MetaData = {
    val {cls, method} = splitColonPair(name, null, "render")
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

  private def processAttributes(in: MetaData, request: RequestState, finder: (String) => InputStream) : MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
        mine.key match {
          case "snippet" => findAttributeSnippet(mine.value.text, request, finder, processAttributes(in.next, request, finder))
          case _ => processAttributes(in.next, request, finder)
        }
      }
      case notMine => notMine.copy(processAttributes(in.next, request, finder))
    }
  }
  
  private def findSnippet(snippetName: Option[Seq[Node]], kids: NodeSeq, request: RequestState, finder: (String) => InputStream) : NodeSeq = {
    snippetName match {
      case None => kids
      case Some(ns) => {
        val {cls, method} = splitColonPair(ns.text, null, "render")
        findSnippetClass(cls) match {
	  case None => kids
	  case Some(clz) => {
            invokeMethod(clz, method) match {
              case Some(md: NodeSeq) => md
              case _ => kids
            }
	  }

          
        }
      }
    }
  }
  
  private def processSurroundAndInclude(in : NodeSeq, session : RequestState, finder: (String) => InputStream) : NodeSeq = {
    in.flatMap{
      v => 
        v match {
          case Elem("lift", "surround", attr @ _, _, kids @ _*) => {findAndMerge(attr.get("with"), attr.get("at"), processSurroundAndInclude(kids, session, finder), session, finder)}
          case Elem("lift", "embed", attr @ _, _, kids @ _*) => {findAndImbed(attr.get("what"), processSurroundAndInclude(kids, session, finder), session, finder)}
          case Elem("lift", "snippet", attr @ _, _, kids @ _*) => {findSnippet(attr.get("type"), processSurroundAndInclude(kids, session, finder), session, finder)}
          case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, processAttributes(v.attributes, session, finder), v.scope, processSurroundAndInclude(v.child, session, finder) : _*)}
          case _ => {v}
        }
    }
  }
  
  def findAndMerge(templateName : Option[Seq[Node]], at : Option[Seq[Node]], kids : NodeSeq, session : RequestState, finder: (String) => InputStream) : NodeSeq = {
    val name : String = templateName match {
      case None => "/templates-hiddend/Default"
      case Some(s) => if (s.text.startsWith("/")) s.text else "/"+ s.text
    }
    
    findTemplate(name, session, finder) match {
      case None => kids
      case Some(s) => processBind(processSurroundAndInclude(s, session, finder), at match {case None => "main"; case _ => at.get.text}, kids)
    }
  }

  
  /*
  private def serviceRequestWithTemplate(session: RequestState) : Option[Response] = {
    findVisibleTemplate(session.uri, session) match {
      case None => None
  case Some(s) => {
    processTemplate(s, session) match {
      case None => None
  case s @ Some(_) => s
    }
  }
    }
  }
  
  private def serviceRequest(session: RequestState) : Response = {
    serviceRequestWithTemplate(session) match {
      case Some(s) => s
  case None => doRailsStyleDispatch(session) match {
    case None => session.createNotFound
  case Some(s) => s
  }
    }
  }*/
  

  

  private val suffixes = List("xhtml", "htm", "_html", "xml", "html", "")
  
  def findAnyTemplate(places : List[String], session : RequestState, finder: (String) => InputStream) : Option[NodeSeq] = {
    val toTry = (List.range(0, places.length).map {
      i =>
        places.take(i + 1).mkString("/","/","")
    }).flatMap {
      i =>
        suffixes.map {
          s =>
            i + (if (s.length > 0) "." + s else "")
        }
    }.reverse
    
    first(toTry) {
      s => 
        finder(s) match {
          case null => None
          case found => PCDataXmlParser(found)
        }
    } orElse {lookForClasses(places)}
  }
  
  def lookForClasses(places : List[String]) : Option[NodeSeq] = {
    val trans = List({(n:String) => n}, {(n:String) => smartCaps(n)})
    val toTry = List.range(0, places.length).flatMap {
      i =>
	trans.flatMap{
          f =>
            val what = places.take(i + 1).map{st => f(st)}.mkString("", ".", "")
	  (buildPackage("view") ::: ("lift.app.view" :: Nil)).map{pkg => pkg + "."+what}
    }
				     }

    first(toTry) {
      clsName => 
	try {
          tryn(List(classOf[ClassNotFoundException])) {

            Class.forName(clsName)
          } match {
            case null => None
            case c @ _ => {
              val inst = c.newInstance
              c.getMethod("render", null).invoke(inst, null) match {
		case null | None => None
		case s : NodeSeq => Some(s)
		case Some(n : Seq[Node]) => Some(n)
		case Some(n : NodeSeq) => Some(n)
		case _ => None
              }
            }
          }
	} catch {
          case _ => None
	}
    }
  }
  
  



  /**
  * Look for a class that matches the URL pattern and
  * calls that class
  */
  /*
  private def doRailsStyleDispatch(session : RequestState) : Option[Response] = {
    // get the first element (the controller)
    val controller = session.controller
  // get the second element (the view, and put in 'index' if it doesn't exist)
  val page = session.view

  // invoke the named controller and view
  invokeControllerAndView(controller, page, session)
  }*/
  
  /*
  /**
  * Invoke the named controller and optionally its view
  */
  private def invokeControllerAndView(controller : String, page : String, session: RequestState) : Option[Response] = {
    // invoke the controller
    invokeController(controller, page, session) match {
      case Some(r : Response) => Some(r)
      case s: Response => Some(s) // if it returns a Response, return that response
      case s: NodeSeq => Some(Response(s, null, 200)) // if it returns an XML (XHTML) object, make a response out of it
      case false => None  // if it returns 'false' we don't go on to running the view
      case f : Function0[_] => dealWithViewRet(f.apply)
      
      case s: Map[_, _] => invokeView(controller, page, session) // run a view associated with the controller
      case _ => None
    }
    

  }
  


  
  /**
  * invoke the named controller
  */
  private def invokeController(controller : String, page : String, session: RequestState) : Any = {
    // find the controller
    findClass(controller, "lift.app.controller" :: "base.app.controller" :: "app.controller" :: Nil) match {
      case None => {null}
      case Some(clz) => {
        if (classHasControllerMethod(clz, page)) {
          invokeControllerMethod(clz, page) // invoke it
        } else null
      }
    }
  }
  
  private def dealWithViewRet(in : Any) : Option[Response] = {
    in match  {
      case Some(r: Response) => Some(r)
      case s: Response => Some(s) // if it returns a Response, return that response
      case s: NodeSeq => Some(Response(s, null, 200)) // if it returns an XML (XHTML) object, make a response out of it
      case _ => None
    }
  }*/
  
  /*
  // invoke the "render" method on the named view
  private def invokeView(controller : String, page : String, session: RequestState ) : Option[Response] = {
    findClass(page, "app.view."+controller :: "base.app.view."+controller :: "lift.app.view."+controller :: Nil) match { 
      case None => None
  case Some(v) => invokeRenderMethod(v)
    }
  }*/
  
  /*
  def invokeRenderMethod(clz : Class) : Option[Response] = {
    if (clz == null)  null else {
      try {
        val meth = clz.getMethod("render", null)
  if (!callableMethod_?(meth) ) null else {
    dealWithViewRet(meth.invoke(clz.newInstance, null))
  }
      } catch {
        case c : InvocationTargetException => {def findRoot(e : Throwable) : Option[Response] = {if (e.getCause == null || e.getCause == e) throw e else findRoot(e.getCause)}; findRoot(c)}
      }
    }
  }*/

}

abstract class SessionMessage

case class Setup(page: NodeSeq, http: Actor) extends SessionMessage
case class SetGlobal(name: String, value: Any) extends SessionMessage
case class UnsetGlobal(name: String) extends SessionMessage
/**
 * Sent from a session to a Page to tell the page to render itself and includes the sender that
 * the rendered response should be sent to
 */
case class AskRenderPage(state: RequestState, sessionState: TreeMap[String, Any], sender: Actor, controllerMgr: ControllerManager, timeout: long) extends SessionMessage

/**
 * The response from a page saying that it's been rendered
 */
case class AnswerRenderPage(state: RequestState, thePage: Response, sender: Actor) extends SessionMessage
case class AskSessionToRender(request: RequestState,finder: (String) => InputStream,timeout: long)

/*
 case class RequestPending(url: String,
 pathPrefix: String,
 attributes: Map[String, List[String]],
 parameters: Map[String, List[String]],
 headers: Map[String, List[String]],
 method: String) extends SessionMessage
 case class RequestUnpending extends SessionMessage
 case class TheRendering(content: Array[byte],
 headers: List[{String, String}],
 response: int) extends SessionMessage
 */


