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
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.collection.immutable.{ListMap}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment}

class Session extends Actor with HttpSessionBindingListener {
  private val pages = new HashMap[String, Page]
  private var sessionState: TreeMap[String, Any] = TreeMap.empty
  private var running_? = false
        
  /**
    * What happens when this controller is bound to the HTTP session?
    */ 
  def valueBound(event: HttpSessionBindingEvent) {
    // ignore this event  
  }

  /**
    * When the session is unbound the the HTTP controller, stop us
    */
  def valueUnbound(event: HttpSessionBindingEvent) {
    // stop running
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
    
    case r : RequestState => 
      processRequest(r)
      loop
      
    case RenderedPage(state: RequestState, thePage: Response, sender: Actor) =>
      val updatedPage = fixResponse(thePage, state)
      sender ! Some(updatedPage)
      loop

    case SetGlobal(name, value) => sessionState = (sessionState(name) = value); loop
    case UnsetGlobal(name) => sessionState = (sessionState - name); loop
    case _ => loop
  }
  
  private def processRequest(state: RequestState) = {
    
    val page = pages.get(state.uri) match {
      case None => {
        createPage(state) match {
          case None => None
          case s @ Some(p: Page) =>
            pages(state.uri) = p
            s
        }
      }
      case Some(p : Page) => {
        findVisibleTemplate(state.uri, state) match {
          case Some(xml: NodeSeq) => p ! SetupPage(processSurroundAndInclude(xml, state), this) // FIXME reloads the page... maybe we don't always want to do this
          case _ => {}
        }
        Some(p)
      }
    }
    
    page match {
      case Some(p) => p ! RenderPage(state, sessionState, sender)
      case _ => sender ! Some(state.createNotFound)
    }
  }

  /**
    * Create a page based on the uri
    */
  private def createPage(state: RequestState): Option[Page] = {
    findVisibleTemplate(state.uri, state) match {
      case None => None
      case Some(xml: NodeSeq) => {
        val ret = new Page // FIXME we really want a Page factory here so we can build other Page types
        ret.link(self)
        ret.start
        val realXml : NodeSeq = processSurroundAndInclude(xml, state)
        ret ! SetupPage(realXml, this)
        Some(ret)
      }
    }
  }
  
  private def findVisibleTemplate(name : String, session : RequestState) : Option[NodeSeq] = {
    val splits = name.split("/").toList.drop(1) match {
      case s @ _ if (!s.isEmpty) => s.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("template") == -1}
      case _ => List("index")
    }
    
    findAnyTemplate(splits, session)
  }
  
  private def findTemplate(name : String, session : RequestState) : Option[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    
    findAnyTemplate(splits, session) match {
      case s @ Some(_) => s
      case None => findAnyTemplate("template" :: splits, session)
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
  
  private def findAndImbed(templateName : Option[Seq[Node]], kids : NodeSeq, session : RequestState) : NodeSeq = {
    templateName match {
      case None => Comment("FIX"+"ME No named specified for embedding") concat kids
      case Some(s) => {
        findTemplate(s.text, session) match {
          case None => Comment("FIX"+"ME Unable to find template named "+s.text) concat kids
          case Some(s) => processSurroundAndInclude(s, session)
        }
      }
    }
  }

  
  private def processSurroundAndInclude(in : NodeSeq, session : RequestState) : NodeSeq = {
    in.flatMap{
      v => 
        v match {
          case Elem("mondo", "surround", attr @ _, _, kids @ _*) => {findAndMerge(attr.get("with"), attr.get("at"), processSurroundAndInclude(kids, session), session)}
          case Elem("mondo", "embed", attr @ _, _, kids @ _*) => {findAndImbed(attr.get("what"), processSurroundAndInclude(kids, session), session)}
          case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processSurroundAndInclude(v.child, session) : _*)}
          case _ => {v}
        }
    }
  }
  
  def findAndMerge(templateName : Option[Seq[Node]], at : Option[Seq[Node]], kids : NodeSeq, session : RequestState) : NodeSeq = {
    val name : String = templateName match {
      case None => "/template/Default"
      case Some(s) => if (s.text.startsWith("/")) s.text else "/"+ s.text
    }
    
    findTemplate(name, session) match {
      case None => kids
      case Some(s) => processBind(processSurroundAndInclude(s, session), at match {case None => "main"; case _ => at.get.text}, kids)
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
  
  def findAnyTemplate(places : List[String], session : RequestState) : Option[NodeSeq] = {
    val toTry = (List.range(0, places.length).map {
      i =>
        places.take(i + 1).mkString("/","/","")
    }).flatMap {
      i =>
        suffixes.map {
          s =>
            i + (if (s.length > 0) "." + s else "")
        }
    }
    
    first(toTry) {
      s => 
        session.resourceFinder(s) match {
          case null => None
          case s @ _ => Some(s)
        }
    } match {
      case None => lookForClasses(places)
      case Some(s) => Some(XML.load(s))
    }
  }
  
  def lookForClasses(places : List[String]) : Option[NodeSeq] = {
    val toTry = List.range(0, places.length).map {
      i =>
	"mondo.app.view."+ (places.take(i + 1).mkString("", ".", ""))
    }
    first(toTry) {
      clsName => 
	try {
          Class.forName(clsName) match {
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
    findClass(controller, "mondo.app.controller" :: "base.app.controller" :: "app.controller" :: Nil) match {
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
    findClass(page, "app.view."+controller :: "base.app.view."+controller :: "mondo.app.view."+controller :: Nil) match { 
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
case class RenderPage(state: RequestState, sessionState: TreeMap[String, Any], sender: Actor) extends SessionMessage

/**
  * The response from a page saying that it's been rendered
  */
case class RenderedPage(state: RequestState, thePage: Response, sender: Actor) extends SessionMessage
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