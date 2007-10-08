package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent, HttpSession}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.xml.{NodeSeq, Unparsed, Text}
import net.liftweb.mapper.DB
import net.liftweb.util._
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import java.io.InputStream
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
// import net.liftweb.http.S._
import scala.xml.transform._

object LiftSession {

  def createSession[T](session: HttpSession, uri: String,
		       path: ParsePath,
		       contextPath: String,
		       requestType: RequestType,
		       webServices_? : boolean,
		       contentType: String, opts: Can[T]) = {new LiftSession(uri, path, contextPath, requestType, webServices_?, contentType)}
  
  var creator = createSession _
  
  def apply[T](session: HttpSession, uri: String,
	       path: ParsePath,
	       contextPath: String,
	       requestType: RequestType,
	       webServices_? : boolean,
	       contentType: String, opts: Can[T]) = creator(session, uri, path, contextPath, requestType, webServices_?, contentType, opts)
}

@serializable
class LiftSession(val uri: String,val path: ParsePath,val contextPath: String, val requestType: RequestType, val webServices_? : boolean, val contentType: String) extends Actor with HttpSessionBindingListener with HttpSessionActivationListener {
  private var running_? = false
  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap
  private var notices: Seq[(NoticeType.Value, NodeSeq)] = Nil
  private var _state: Map[String, String] = Map.empty
  private val asyncComponents = new HashMap[(Can[String], Can[String]), CometActor]()
  private val asyncById = new HashMap[String, CometActor]()
  
  def sessionDidActivate(se: HttpSessionEvent) = {
    
  }
  def sessionWillPassivate(se: HttpSessionEvent) = {
    
  }
  
  private var cometList: List[Actor] = Nil
  
  private[http] def breakOutComet(): Unit = synchronized {
    cometList.foreach(_ ! BreakOut)
  }
  
  private[http] def enterComet(what: Actor): Unit = synchronized {
    cometList = what :: cometList
  }
  
  private[http] def exitComet(what: Actor): Unit = synchronized {
    cometList = cometList.remove(_ eq what)
  }
  
  // def callbacks = messageCallback
  
  /**
   * What happens when this session is bound to the HTTP session?
   */ 
  def valueBound(event: HttpSessionBindingEvent) {
    
  }
  
  def runParams(state: RequestState): List[Any] = {
    val toRun = synchronized {
      // get all the commands, sorted by owner, 
      state.paramNames.filter(n => messageCallback.contains(n)).map{n => val mcb = messageCallback(n);  (n, mcb, mcb.owner)}.
      sort{
	case ( (_, _, Full(a)) , (_, _, Full(b))) if a < b => true 
	case ((_, _, Full(a)), (_, _, Full(b))) if a > b => false 
	case ((an, _, Full(a)), (bn, _, Full(b))) if a == b => an < bn
	case ((_,_, Full(_)), _) => false
	case (_, (_, _, Full(_))) => true
	case ((a, _, _), (b, _, _)) => a < b
      }
    }
    
    val ret = toRun.map(_._3).removeDuplicates.flatMap{w => 
      val f = toRun.filter(_._3 == w);
      w match {
	// if it's going to a CometActor, batch up the commands
	case Full(id) => asyncById.get(id).toList.flatMap(a => a !? ActionMessageSet(f.map(tf => ActionMessage(tf._2, state.params(tf._1),a, _state)), _state) match {case Some(li: List[Any]) => li case li: List[Any] => li case other => Nil})
	case _ => f.map(i => i._2(state.params(i._1)))
	}
      }
    
    ret
  }
  
  
  def updateFunctionMap(funcs: Map[String, S.AFuncHolder], uniqueId: String, when: Long): Unit = synchronized {
    funcs.foreach{case (name, func) => messageCallback(name) = func.duplicate(uniqueId)}
    }

  /**
   * When the session is unbound the the HTTP session, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    try {
    if (running_?) this ! ShutDown
    } finally {
      // uncomment for Scala 2.6.1 to avoid memory leak 
      // Actor.clearSelf
      DB.clearThread
    }
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
    * Called just before the session exits.  If there's clean-up work, override this method 
    */
  def cleanUpSession() {
     
   }
  
  def dispatcher: PartialFunction[Any, Unit] = {
    case ShutDown =>
      Log.debug("Shutting down session")
    asyncComponents.foreach{case (_, comp) => comp ! ShutDown}
      cleanUpSession()
    self.exit
    
    case AskSessionToRender(request,httpRequest, timeout, whenDone) => processRequest(request, httpRequest, timeout, whenDone)

    case UpdateState(name, Full(value)) => stateVar(name) = value

    case UpdateState(name, _) => stateVar - name
    
    case CurrentVars => reply(_state)

    case unknown => Log.debug("LiftSession Got a message "+unknown)
  }
  
  object stateVar {
    def apply(name: String): Can[String] = Can(_state.get(name))
    def -(name: String): Unit = _state = _state - name
    def update(name: String, value: String): Unit = _state = _state + name -> value
  }
  
  private def processRequest(request: RequestState, httpRequest: HttpServletRequest, timeout: Long, whenDone: AnswerHolder => Any) = synchronized {
    S.init(request, httpRequest, notices,this, new VarStateHolder(this, this._state, Empty, true)) {
      try {
	val sessionDispatch = S.highLevelSessionDispatcher
	val toMatch = RequestMatcher(request, request.path, this)        
	if (sessionDispatch.isDefinedAt(toMatch)) {
	  runParams(request)
	  sessionDispatch(toMatch)(httpRequest) match {
	    case Full(r) => whenDone(AnswerHolder(r))
	    case _ => whenDone(AnswerHolder(request.createNotFound))
	  }
	  /*if (!request.ajax_?)*/ notices = Nil
	} else {
	  // make sure we're okay, sitemap wise
	  request.testLocation.foreach{s => S.error(s.msg); S.redirectTo(s.to)} 
	  
	  runParams(request)

	  def idAndWhen(in: Node): Can[(String, String)] = 
	    ((in \ "@id").toList, in.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList) match {
	      case (x :: _, y :: _) => Full((x.text,y.value.text))
	      case _ => Empty
	    }
	  
	  findVisibleTemplate(request.path, request).map(xml => processSurroundAndInclude(xml)) match {
	    case Full(xml: NodeSeq) => {
	      val realXml = (xml \\ "span").filter(!_.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList.isEmpty).toList match {
		case Nil => xml
		case xs => val comets: List[(String, String)] = xs.flatMap(x => idAndWhen(x))
		val cometVar = "var lift_toWatch = "+comets.map{case (a,b) => ""+a+": '"+b+"'"}.mkString("{", " , ", "}")+";"
		val hasJQuery: Boolean = !(xml \\ "script").toList.filter(s => (s \ "@src").toList.map(_.text).mkString("").toLowerCase.indexOf("jquery") >= 0).isEmpty
		
		
		
		val xform = new RuleTransformer(new AddScriptToBody(cometVar) :: (if (!hasJQuery) List(new AddScriptTag) else Nil) :_*)
		xform.transform(xml)
	      }
	      
	      this.synchronized {
		S.functionMap.foreach(mi => messageCallback(mi._1) = mi._2)
	      }
	      notices = Nil
	      whenDone(AnswerHolder(XhtmlResponse(Group(request.fixHtml(realXml)), ResponseInfo.xhtmlTransitional, Nil, 200)))
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
  
  
  private def findVisibleTemplate(path: ParsePath, session : RequestState) : Can[NodeSeq] = {
    val toMatch = RequestMatcher(session, session.path, this)
    val templ = LiftServlet.templateTable
    (if (templ.isDefinedAt(toMatch)) templ(toMatch)() else Empty) match {
      case ns @ Full(_) => ns 
      case _ =>
	val tpath = path.path
      val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
	case s @ _ if (!s.isEmpty) => s
	case _ => List("index")
      }
      findAnyTemplate(splits)
    }
  }
  
  def currentVars: Map[String, String] = (this !? (500L, CurrentVars)) match {
    case Some(s: Map[String, String]) => s
    case _ => Map.empty
  }
  
  private def findTemplate(name: String) : Can[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }
    
    findAnyTemplate(splits) or findAnyTemplate("templates-hidden" :: splits)
  }
  

  private val suffixes = List("", "html", "xhtml", "htm")
  
  private def findAnyTemplate(places : List[String]) : Can[NodeSeq] = {
    val pls = places.mkString("/","/", "")
    val toTry = suffixes.map(s => pls + (if (s.length > 0) "." + s else ""))
    
    first(toTry)(LiftServlet.finder(_).flatMap(PCDataXmlParser(_))) or lookForClasses(places)
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
      case _ => in.ciGet("Content-Type").map(_.toLowerCase.contains("html")) openOr true
    }
  }
  
  /*
  def fixResponse(resp: XhtmlResponse, request: RequestState): XhtmlResponse = {
    val newHeaders = fixHeaders(resp.headers, request)
    val (newXml, theType) = 
      if (couldBeHtml(resp.headers) && request.contextPath.length > 0) (request.fixHtml(resp.out), ResponseInfo.xhtmlTransitional)
      else (resp.out, Empty)
	
      XhtmlResponse(resp.out, theType, newHeaders, resp.code)
  }*/
  
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
  
  
  private def findAndEmbed(templateName : Can[Seq[Node]], kids : NodeSeq) : NodeSeq = {
    templateName match {
      case Full(s) => {
	findTemplate(s.text) match {
	  case Full(s) => synchronized {processSurroundAndInclude(s)}
	  case _ => Comment("FIX"+"ME Unable to find template named "+s.text) ++ kids
	}
      }
      case _ => Comment("FIX"+"ME No named specified for embedding") ++ kids
    }
  }
  
  private def findSnippetClass(name: String): Can[Class] = {
    if (name == null) Empty
    else findClass(name, buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }
  
  private def findAttributeSnippet(name: String, rest: MetaData): MetaData = {
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

  private def processAttributes(in: MetaData) : MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
	mine.key match {
	  case "snippet" => findAttributeSnippet(mine.value.text, processAttributes(in.next))
	  case _ => mine.copy(processAttributes(in.next))
	}
      }
      case notMine => notMine.copy(processAttributes(in.next))
    }
  }
  
  private def processSnippet(snippetName: Can[Seq[Node]], attrs: MetaData, kids: NodeSeq) : NodeSeq = {
    val ret = snippetName match {
      case Full(ns) => {
	S.locateSnippet(ns.text).map(_(kids)) openOr {
	  val (cls, method) = splitColonPair(ns.text, null, "render")
	  findSnippetClass(cls) match {
	    case Empty => kids
	    case Full(clz) => {
              val ar: Array[Object] = List(Group(kids)).toArray
	      ((invokeMethod(clz, method, ar)) or invokeMethod(clz, method)) match {
		case Full(md: NodeSeq) => processSurroundAndInclude(md)
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
  
  /*
  def processXHTML(in: NodeSeq): NodeSeq = synchronized {
    val intm = processSurroundAndInclude(in)
    println("in "+in+" intm "+intm)
    println("contextPath "+contextPath)
    RequestState.fixHtml(contextPath, intm)
  }
    */
      
  def fixHtml(in: NodeSeq): NodeSeq = RequestState.fixHtml(contextPath, in)
  
  def processSurroundAndInclude(in : NodeSeq) : NodeSeq = {
    in.flatMap{
      v => 
	v match {
	  case Group(nodes) => Group(processSurroundAndInclude(nodes))
	  case Elem("lift", "comet", attr @ _, _, kids @ _*) => processSurroundAndInclude(executeComet(Can(attr.get("type").map(_.text)), Can(attr.get("name").map(_.text)), processSurroundAndInclude(kids), attr))       
	  case Elem("lift", "ignore", attr @ _, _, kids @ _*) => Text("")
	  case Elem("lift", "surround", attr @ _, _, kids @ _*) => processSurroundElement(v.asInstanceOf[Elem])
	  case Elem("lift", "embed", attr @ _, _, kids @ _*) => findAndEmbed(Can(attr.get("what")), processSurroundAndInclude(kids))
	  case Elem("lift", "snippet", attr @ _, _, kids @ _*) => S.setVars(attr)(processSnippet(Can(attr.get("type")), attr, processSurroundAndInclude(kids)))
	  case Elem("lift", "children", attr @ _, _, kids @ _*) => processSurroundAndInclude(kids)
	  case Elem("lift", "vars", attr @ _, _, kids @ _*) => S.setVars(attr)(processSurroundAndInclude(kids))
	  case Elem("lift", "a", attr @ _, scope @ _, kids @ _*) => Elem(null, "a", addAjaxHREF(attr), scope, processSurroundAndInclude(kids): _*)
	  case Elem("lift", "form", attr @ _, scope @ _, kids @ _*) => Elem(null, "form", addAjaxForm(attr), scope, processSurroundAndInclude(kids): _*)
	  case Elem(_,_,_,_,_*) => Elem(v.prefix, v.label, processAttributes(v.attributes), v.scope, processSurroundAndInclude(v.child) : _*)
	  case _ => {v}
	}
    }
  }
  
  private def executeComet(theType: Can[String], name: Can[String], kids: NodeSeq, attr: MetaData): NodeSeq = {
    try {
      findComet(theType, name, kids, Map.empty ++ attr.flatMap{case u: UnprefixedAttribute => List((u.key, u.value.text)) case u: PrefixedAttribute => List((u.pre+":"+u.key, u.value.text)) case _ => Nil}.toList).
      map(c =>
	(c !? (600, AskRender)) match {
	  case Some(AnswerRender(response, _, when)) => <span id={c.uniqueId} lift:when={when.toString}>{response.asXhtml}</span>
	  case _ => <span id={c.uniqueId} lift:when="0">{Comment("FIX"+"ME comet type "+theType+" name "+name+" timeout") ++ kids}</span>
	}) openOr Comment("FIX"+"ME - comet type: "+theType+" name: "+name+" Not Found ") ++ kids
    } catch {
      case e => e.printStackTrace; kids
    }
  }  
  
  def findComet(theType: String): List[CometActor] = synchronized {
    asyncComponents.elements.filter{case ((Full(name), _), _) => name == theType case _ => false}.toList.map{case (_, value) => value} 
  }
  
  private def findComet(theType: Can[String], name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]): Can[CometActor] = {
    val what = (theType, name)
      Can(asyncComponents.get(what)).or( {
	theType.flatMap{
	  tpe =>
	    val ret = findCometByType(tpe, name, defaultXml, attributes)
	  ret.foreach(r => 
	    synchronized {
	      asyncComponents(what) = r
	      asyncById(r.uniqueId) = r
	    })
	  ret
	}
      })
  }
  
  def getAsyncComponent(id: String): Can[CometActor] = synchronized(asyncById.get(id))
  
  def addCometActor(act: CometActor): Unit = synchronized {
    asyncById(act.uniqueId) = act
  }
  
  def removeCometActor(act: CometActor): Unit = synchronized {asyncById -= act.uniqueId
    // FIXME remove all the stuff from the function table related to this item
    
    }
  
  
  private def findCometByType(contType: String, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]): Can[CometActor] = {
    findClass(contType, buildPackage("comet") ::: ("lift.app.comet" :: Nil), {c : Class => classOf[CometActor].isAssignableFrom(c)}).flatMap{
      cls =>
	tryo {
	  val constr = cls.getConstructor(List(classOf[LiftSession], classOf[Can[String]], classOf[NodeSeq], classOf[Map[String, String]]).toArray)
	  val ret = constr.newInstance(List(this, name, defaultXml, attributes).toArray).asInstanceOf[CometActor];
	  ret.start
	  ret.link(this)
	  ret ! PerformSetupComet(_state)
	  ret
	}
    }
  }  
  
  
  
  private def addAjaxHREF(attr: MetaData): MetaData = {
    val ajax = "jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+attr("key")+"=true', dataType: 'script'});"
    new UnprefixedAttribute("onclick", ajax, new UnprefixedAttribute("href", "#", Null))
  }

  private def addAjaxForm(attr: MetaData): MetaData = {
    val id = "F"+randomString(15)
    val ajax = "jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: jQuery('#"+id+"').serialize(), dataType: 'script', type: 'POST'}); return false;"
    new UnprefixedAttribute("id", id, new UnprefixedAttribute("action", "#", new UnprefixedAttribute("onsubmit", ajax, Null))) //  new UnprefixedAttribute("method", "POST", Null)))
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
  
  private def processSurroundElement(in : Elem) : NodeSeq = in match {
    case Elem("lift", "surround", attr @ _, _, kids @ _*) =>
      
      val (otherKids, paramElements) = filter2(kids) {
	case Elem("lift", "with-param", _, _, _) => false
	case _ => true
      }
    
    val params = paramElements.flatMap {
      case Elem("lift", "with-param", attr @ _, _, kids @ _*) =>
	val valueOption: Option[Seq[Node]] = attr.get("name")
      val option: Option[(String, NodeSeq)] = valueOption.map((v: Seq[Node]) => (v.text, processSurroundAndInclude(kids)))
      option
    }
    
    val mainParam = (attr.get("at").map(_.text: String).getOrElse("main"),
		     processSurroundAndInclude(otherKids))
      val paramsMap = collection.immutable.Map(params: _*) + mainParam
    findAndMerge(attr.get("with"), paramsMap)
  }
  
  private def findAndMerge(templateName : Can[Seq[Node]], atWhat : Map[String, NodeSeq]) : NodeSeq = {
    val name : String = templateName match {
      case Full(s) => if (s.text.startsWith("/")) s.text else "/"+ s.text
      case _ => "/templates-hidden/Default"
    }
    
    findTemplate(name) match {
      case Full(s) => processBind(processSurroundAndInclude(s), atWhat)
      case _ => atWhat.values.flatMap(_.elements).toList
    }
  }
  
  class AddScriptTag extends RewriteRule {
    override def transform(n: Node) = n match {
      case Elem(null, "head", attr @ _, scope @ _, kids @ _*) =>
        Elem(null, "head", attr,  scope, (kids ++ <script src="/scripts/jquery-cur.min.js" type="text/javascript"/>) :_*)
      case _ => n
    }
  }

  class AddScriptToBody(val cometVar: String) extends RewriteRule {
    override def transform(n: Node) = n match {
      case Elem(null, "body", attr @ _, scope @ _, kids @ _*) =>
        Elem(null, "body", attr,  scope, (kids ++ <span id="lift_bind"/><script>
   // { Unparsed("""<![CDATA[
   """+cometVar+"""
   function lift_handlerSuccessFunc() {setTimeout("lift_cometEntry();",100);}
   function lift_handlerFailureFunc() {setTimeout("lift_cometEntry();",10000);}
   function lift_cometEntry() {jQuery.ajax( {url: '"""+contextPath+"/"+LiftServlet.cometPath+"""', cache: false, success: lift_handlerSuccessFunc, data: lift_toWatch, dataType: 'script', error: lift_handlerFailureFunc} );}
   jQuery(document).ready(function(){lift_handlerSuccessFunc();});
   // ]]>
   """)}</script>) :_*)
      case _ => n
    }
  }                                                                                                 
}

abstract class SessionMessage


/**
 * The response from a page saying that it's been rendered
 */
case class AskSessionToRender(request: RequestState,httpRequest: HttpServletRequest,timeout: Long, sendBack: AnswerHolder => Any)
case class UpdateState(name: String, value: Can[String]) extends SessionMessage
case object CurrentVars extends SessionMessage
case object ShutDown

case class AnswerHolder(what: ResponseIt)

// vim: set ts=2 sw=2 et:
