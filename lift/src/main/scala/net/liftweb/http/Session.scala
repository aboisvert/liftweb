package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.xml.{NodeSeq, Unparsed, Text}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import java.io.InputStream
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
import net.liftweb.http.S._

object Session {
  def createSession( uri: String,
      path: ParsePath,
       contextPath: String,
       requestType: RequestType,
       webServices_? : boolean,
       contentType: String) = {new Session(uri, path, contextPath, requestType, webServices_?, contentType)}
       
   var creator = createSession _
   
   def apply(uri: String,
      path: ParsePath,
       contextPath: String,
       requestType: RequestType,
       webServices_? : boolean,
       contentType: String) = creator(uri, path, contextPath, requestType, webServices_?, contentType)
}

class Session(val uri: String,
           val path: ParsePath,
           val contextPath: String,
           val requestType: RequestType,
           val webServices_? : boolean,
           val contentType: String) extends Actor with HttpSessionBindingListener with HttpSessionActivationListener {
  private val pages = new HashMap[String, Page]
  // private var sessionState: TreeMap[String, Any] = TreeMap.empty
  private var running_? = false
  private var messageCallback: HashMap[String, AFuncHolder] = new HashMap
  private var notices: Seq[(NoticeType.Value, NodeSeq)] = Nil
  private var _state: Map[String, String] = Map.empty
  private val theControllerMgr = {
    val ret = new ControllerManager
    ret.start
    ret.link(this)
    ret
  }
  
  val uniqueId = randomString(20)
  
  def sessionDidActivate(se: HttpSessionEvent) = {

  }
  def sessionWillPassivate(se: HttpSessionEvent) = {

  }
  
  /**
   * What happens when this controller is bound to the HTTP session?
   */ 
  def valueBound(event: HttpSessionBindingEvent) {
    
  }

  /**
   * When the session is unbound the the HTTP controller, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    if (running_?) this ! ShutDown
  }
  
  /**
   * called when the Actor is started
   */
  def act = {
    this.trapExit = true
    running_? = true

    loop(react(dispatcher))
  }
  
  /**
   * The loop for the actor.  Dispatches messages using Scala's event-based Actors
   */
  /*final def loop {
    react(dispatcher)
  }*/
  
  def dispatcher: PartialFunction[Any, Unit] = {
    case ShutDown =>
      Log.debug("Shutting down session")
      theControllerMgr ! ShutDown
      pages.foreach(_._2 ! ShutDown)
      self.exit
    
    case AskSessionToRender(request,httpRequest, timeout, whenDone) => 
      processRequest(request, httpRequest, timeout, whenDone)
    
    case AnswerRenderPage(request, thePage, sender) =>
      val updatedPage = fixResponse(thePage, request)
      sender(AnswerHolder(updatedPage))
    
    case SendEmptyTo(sender) =>
      sender(AnswerHolder(XhtmlResponse(Unparsed(""), Empty, List("Content-Type" -> "text/javascript"), 200)))
    

    case UpdateState(name, Full(value)) => stateVar(name) = value

    case UpdateState(name, _) => stateVar - name
    
    case CurrentVars => reply(_state)

    case unknown => Log.debug("Session Got a message "+unknown)
  }
  
  object stateVar {
    def apply(name: String): Can[String] = Can(_state.get(name))
    def -(name: String): Unit = _state = _state - name
    def update(name: String, value: String): Unit = _state = _state + name -> value
  }
  
  private def processParameters(r: RequestState) {
    r.paramNames.filter(n => messageCallback.contains(n)).foreach{
      n => 
            val f = messageCallback(n)
            f(r.params(n))
    }
  }
  
  private def processRequest(request: RequestState, httpRequest: HttpServletRequest, timeout: Long,
      whenDone: AnswerHolder => Any) = {
    S.init(request, httpRequest, notices,this, new VarStateHolder(this, this._state, Empty, true)) {
      try {
        val sessionDispatch = S.highLevelSessionDispatcher
        val toMatch = RequestMatcher(request, request.path)        
        if (sessionDispatch.isDefinedAt(toMatch)) {
          processParameters(request)
          sessionDispatch(toMatch)(httpRequest) match {
            case Full(r) => whenDone(AnswerHolder(r))
            case _ => whenDone(AnswerHolder(request.createNotFound))
          }
          if (!request.ajax_?) notices = Nil
        } else {
          // make sure we're okay, sitemap wise
        request.testLocation.foreach{s => S.error(s.msg); S.redirectTo(s.to)} 
        
        processParameters(request)

        findVisibleTemplate(request.path, request).map(xml => processSurroundAndInclude(xml, request)) match {
          case Full(xml: NodeSeq) => {
            S.getFunctionMap.foreach(mi => messageCallback(mi._1) = mi._2)

            if ((xml \\ "controller").filter(_.prefix == "lift").isEmpty) {
              if (request.ajax_?) {
                ActorPing.schedule(this, SendEmptyTo(whenDone), timeout - 250) 
              } else {
                notices = Nil; whenDone(AnswerHolder(XhtmlResponse(Group(request.fixHtml(xml)),ResponseInfo.xhtmlTransitional, Nil, 200)))
              }
            } else {
              val page = pages.get(request.uri) getOrElse {val p = createPage; pages(request.uri) = p; p }
              page ! AskRenderPage(request, xml, whenDone, theControllerMgr, timeout, _state)
              if (!request.ajax_?) notices = Nil
            }
          }
          case _ => whenDone(AnswerHolder(request.createNotFound))
          
        }
        }
      } catch {
        case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[RedirectException]) =>
        val rd = ite.getCause.asInstanceOf[RedirectException]
        notices = S.getNotices
        
        whenDone(AnswerHolder(XhtmlResponse(Group(request.fixHtml(<html><body>{request.uri} Not Found</body></html>)),
                 ResponseInfo.xhtmlTransitional,
                 List("Location" -> (request.updateWithContextPath(rd.to))),
                 302)))
          case rd : net.liftweb.http.RedirectException => {   
            notices = S.getNotices
            
            whenDone(AnswerHolder(XhtmlResponse(Group(request.fixHtml(<html><body>{request.uri} Not Found</body></html>)), ResponseInfo.xhtmlTransitional,
                     List("Location" -> (request.updateWithContextPath(rd.to))),
                     302)))
          }
	case e  => whenDone(AnswerHolder(request.showException(e)))
      }
    }
  }
  
  /**
   * Create a page based on the uri
   */
  private def createPage: Page = {
        val ret = new Page(this) // FIXME we really want a Page factory here so we can build other Page types
        ret.link(this)
        ret.start
	ret
  }
  
  private def findVisibleTemplate(path: ParsePath, session : RequestState) : Can[NodeSeq] = {
    val toMatch = RequestMatcher(request, request.path)
    val templ = Servlet.templateTable
    (if (templ.isDefinedAt(toMatch)) templ(toMatch)() else Empty) match {
      case ns @ Full(_) => ns 
      case _ =>
    val tpath = path.path
    val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
      case s @ _ if (!s.isEmpty) => s
      case _ => List("index")
    }
    findAnyTemplate(splits, session)
    }
  }
  
  def currentVars: Map[String, String] = (this !? (500L, CurrentVars)) match {
    case Some(s: Map[String, String]) => s
    case _ => Map.empty
  }
  
  private def findTemplate(name: String, session : RequestState) : Can[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }
    
    findAnyTemplate(splits, session) or findAnyTemplate("templates-hidden" :: splits, session)
  }
  

  private val suffixes = List("", "html", "xhtml", "htm")
  
  private def findAnyTemplate(places : List[String], session : RequestState) : Can[NodeSeq] = {
    val pls = places.mkString("/","/", "")
    val toTry = suffixes.map(s => pls + (if (s.length > 0) "." + s else ""))
    
    first(toTry)(session.finder(_).flatMap(PCDataXmlParser(_))) or lookForClasses(places)
  }  
  
  private def lookForClasses(places : List[String]) : Can[NodeSeq] = {
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
                case null | Empty => Empty
                case s : NodeSeq => Full(s)
                case Full(n : NodeSeq) => Full(n)
                case Full(n : Seq[Node]) => Full(n)
                case _ => Empty
              }
            }
        } catch {
          case _ => Empty
        }
    }
  }
  
  def couldBeHtml(in : List[(String, String)]) : boolean = {
    in match {
      case null | Nil => true
      case _ => in.ciGet("Content-Type").map(_.toLowerCase == "text/html") openOr true
    }
  }
  
  def fixResponse(resp: XhtmlResponse, request: RequestState): XhtmlResponse = {
    val newHeaders = fixHeaders(resp.headers, request)
    val (newXml, theType) = if (couldBeHtml(resp.headers) && request.contextPath.length > 0) (request.fixHtml(resp.out), ResponseInfo.xhtmlTransitional)
       else (resp.out, Empty)
    XhtmlResponse(resp.out, theType, newHeaders, resp.code)
  }
  
  /**
   * Update any "Location" headers to add the Context path
   */
  def fixHeaders(h: List[(String, String)], request: RequestState): List[(String, String)] =
    h match {
      case null => Nil
      case _ => h.map{
        case ("Location", v) if (v != null && v.startsWith("/")) => ("Location", "/"+request.contextPath+v)
        case (a, b) => (a, b)
        }
    }
  
  
  private def findAndEmbed(templateName : Can[Seq[Node]], kids : NodeSeq, session : RequestState) : NodeSeq = {
    templateName match {
      case Full(s) => {
        findTemplate(s.text, session) match {
          case Full(s) => processSurroundAndInclude(s, session)
          case _ => Comment("FIX"+"ME Unable to find template named "+s.text) ++ kids
        }
      }
      case _ => Comment("FIX"+"ME No named specified for embedding") ++ kids
    }
  }
  
  
  private def findControllerByType(contType: Can[Seq[Node]]): Can[ControllerActor] = {
    contType.flatMap(ct =>
      findClass(ct.text, buildPackage("controller") ::: ("lift.app.controller" :: Nil), {c : Class => classOf[ControllerActor].isAssignableFrom(c)}) match {
        case Full(cls) => {
          try {
            val ret = Full(cls.newInstance.asInstanceOf[ControllerActor])
            ret
          } catch {
            case _ => Empty
          }
        }
        case _ => Empty
      })
  }
  
  private def findSnippetClass(name: String): Can[Class] = {
    if (name == null) Empty
    else findClass(name, buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }
  
  private def findAttributeSnippet(name: String, request: RequestState, rest: MetaData): MetaData = {
    val (cls, method) = splitColonPair(name, null, "render")
    findSnippetClass(cls) match {
      case Full(clz) =>
        invokeMethod(clz, method) match {
          case Full(md: MetaData) => md.copy(rest)
          case _ => rest
        }
      
      case _ => rest
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
  
  private def processSnippet(snippetName: Can[Seq[Node]], attrs: MetaData, kids: NodeSeq, session : RequestState) : NodeSeq = {
    val ret = snippetName match {
      case Full(ns) => {
          S.locateSnippet(ns.text).map(_(kids)) openOr {
            val (cls, method) = splitColonPair(ns.text, null, "render")
        findSnippetClass(cls) match {
	  case Empty => kids
	  case Full(clz) => {
            ((invokeMethod(clz, method, Array(Group(kids)))) or invokeMethod(clz, method)) match {
              case Full(md: NodeSeq) => processSurroundAndInclude(md, session)
              case _ => kids
            }
	  }
        }
        }
      }
      case _ => kids
    }
    
    attrs.get("form").map(ft => <form action={S.request.uri} method={ft.text}>{ret}</form>) getOrElse ret
  }
  
  private def processSurroundAndInclude(in : NodeSeq, session : RequestState) : NodeSeq = {
    in.flatMap{
      v => 
        v match {
	  case Group(nodes) => Group(processSurroundAndInclude(nodes, session))
          case Elem("lift", "ignore", attr @ _, _, kids @ _*) => Text("")
          case Elem("lift", "surround", attr @ _, _, kids @ _*) => processSurroundElement(v.asInstanceOf[Elem], session)
          case Elem("lift", "embed", attr @ _, _, kids @ _*) => findAndEmbed(Can(attr.get("what")), processSurroundAndInclude(kids, session), session)
          case Elem("lift", "snippet", attr @ _, _, kids @ _*) if (!session.ajax_? || toBoolean(attr("ajax"))) => S.setVars(attr)(processSnippet(Can(attr.get("type")), attr, processSurroundAndInclude(kids, session), session))
          case Elem("lift", "children", attr @ _, _, kids @ _*) => processSurroundAndInclude(kids, session)
          case Elem("lift", "vars", attr @ _, _, kids @ _*) => S.setVars(attr)(processSurroundAndInclude(kids, session))
          case Elem(_,_,_,_,_*) => Elem(v.prefix, v.label, processAttributes(v.attributes, session), v.scope, processSurroundAndInclude(v.child, session) : _*)
          case _ => {v}
        }
    }
  }
  
  /** Split seq into two seqs: first matches p, second matches !p */
  private def filter2[A](c: Seq[A])(p: A => Boolean): (Seq[A], Seq[A]) = {
    val bufs = (new ArrayBuffer[A], new ArrayBuffer[A])
    val i = c.elements
    while (i.hasNext) {
      val x = i.next
      if (p(x)) bufs._1 += x
      else bufs._2 += x
    }
    bufs
  }
  
  private def processSurroundElement(in : Elem, session : RequestState) : NodeSeq = in match {
    case Elem("lift", "surround", attr @ _, _, kids @ _*) =>
      
      val (otherKids, paramElements) = filter2(kids) {
        case Elem("lift", "with-param", _, _, _) => false
        case _ => true
      }
      
      val params = paramElements.flatMap {
        case Elem("lift", "with-param", attr @ _, _, kids @ _*) =>
          val valueOption: Option[Seq[Node]] = attr.get("name")
          val option: Option[(String, NodeSeq)] = valueOption.map((v: Seq[Node]) => (v.text, processSurroundAndInclude(kids, session)))
          option
      }
      
      val mainParam = (attr.get("at").map(_.text: String).getOrElse("main"),
          processSurroundAndInclude(otherKids, session))
      val paramsMap = collection.immutable.Map(params: _*) + mainParam
      findAndMerge(attr.get("with"), paramsMap, session)
  }
  
  private def findAndMerge(templateName : Can[Seq[Node]], atWhat : Map[String, NodeSeq], session : RequestState) : NodeSeq = {
    val name : String = templateName match {
      case Full(s) => if (s.text.startsWith("/")) s.text else "/"+ s.text
      case _ => "/templates-hidden/Default"
    }
    
    findTemplate(name, session) match {
      case Full(s) => processBind(processSurroundAndInclude(s, session), atWhat)
      case _ => atWhat.values.flatMap(_.elements).toList
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
case class AskRenderPage(request: RequestState, xml: NodeSeq, sender: AnswerHolder => Any, controllerMgr: ControllerManager, timeout: Long,
    state: Map[String, String]) extends SessionMessage

/**
 * The response from a page saying that it's been rendered
 */
case class AnswerRenderPage(request: RequestState, thePage: XhtmlResponse,  sender: AnswerHolder => Any) extends SessionMessage
case class AskSessionToRender(request: RequestState,httpRequest: HttpServletRequest,timeout: Long, sendBack: AnswerHolder => Any)
case class SendEmptyTo(who: AnswerHolder => Any) extends SessionMessage
case class UpdateState(name: String, value: Can[String]) extends SessionMessage
case object CurrentVars extends SessionMessage
case object ShutDown

case class AnswerHolder(what: ResponseIt)

// vim: set ts=2 sw=2 et:
