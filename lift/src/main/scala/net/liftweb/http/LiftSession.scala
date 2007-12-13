package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent, HttpSession}
import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
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

  def createSession(session: HttpSession, uri: String,
		       path: ParsePath,
		       contextPath: String,
		       requestType: RequestType,
		       webServices_? : boolean,
		       contentType: String) = {new LiftSession(uri, path, contextPath, requestType, webServices_?, contentType, session)}
  
  var creator = createSession _
  
  def apply(session: HttpSession, uri: String,
	       path: ParsePath,
	       contextPath: String,
	       requestType: RequestType,
	       webServices_? : boolean,
	       contentType: String) = creator(session, uri, path, contextPath, requestType, webServices_?, contentType)
}

@serializable
class LiftSession(val uri: String, val path: ParsePath, val contextPath: String, val requestType: RequestType, val webServices_? : Boolean, val contentType: String, val httpSession: HttpSession) extends /*Actor with */ HttpSessionBindingListener with HttpSessionActivationListener {
  private var running_? = false
  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap
  private var notices: Seq[(NoticeType.Value, NodeSeq)] = Nil
  //  private var _state: Map[String, String] = Map.empty
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
  
  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Can[String]) 
  
  def runParams(state: RequestState): List[Any] = {
    val toRun = synchronized {
      // get all the commands, sorted by owner, 
      (state.uploadedFiles.map(_.name) ::: state.paramNames).filter(n => messageCallback.contains(n)).
        map{n => val mcb = messageCallback(n);  RunnerHolder(n, mcb, mcb.owner)}.
      sort{
	case ( RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a < b => true 
	case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a > b => false 
	case (RunnerHolder(an, _, Full(a)), RunnerHolder(bn, _, Full(b))) if a == b => an < bn
	case (RunnerHolder(_,_, Full(_)), _) => false
	case (_, RunnerHolder(_, _, Full(_))) => true
	case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
      }
    }
    
    def buildFunc(i: RunnerHolder): () => Any = i.func match {
    case bfh: S.BinFuncHolder => () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
    case normal => () => normal(state.params.getOrElse(i.name, state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }
    
    val ret = toRun.map(_.owner).removeDuplicates.flatMap{w => 
      val f = toRun.filter(_.owner == w);
      w match {
	// if it's going to a CometActor, batch up the commands
	case Full(id) => 
        
        asyncById.get(id).toList.flatMap(a => a !? ActionMessageSet(f.map(i => buildFunc(i)), state) match {case Some(li: List[Any]) => li case li: List[Any] => li case other => Nil})
	case _ => f.map(i => buildFunc(i).apply())
	}
      }
    
    ret
  }
  
  
  private[http] def updateFunctionMap(funcs: Map[String, S.AFuncHolder]): Unit = synchronized {
    funcs.foreach(mi => messageCallback(mi._1) = mi._2)
  }
  
  def updateFunctionMap(funcs: Map[String, S.AFuncHolder], uniqueId: String, when: Long): Unit = synchronized {
    funcs.foreach{case (name, func) => messageCallback(name) = func.duplicate(uniqueId)}
    }

  /**
   * When the session is unbound the the HTTP session, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    try {
    if (running_?) this.shutDown
    } finally {
      // uncomment for Scala 2.6.1 to avoid memory leak 
      // Actor.clearSelf
      DB.clearThread
    }
  }
  
  /**
    * Called just before the session exits.  If there's clean-up work, override this method 
    */
  def cleanUpSession() {
     
   }
  
  private def shutDown() = synchronized {
    Log.debug("Shutting down session")
    running_? = false
    asyncComponents.foreach{case (_, comp) => comp ! ShutDown}
    cleanUpSession()
  }

  private[http] def processRequest(request: RequestState, httpRequest: HttpServletRequest): AnswerHolder = {
    S.init(request, httpRequest, notices,this) {
      try {
	val sessionDispatch = S.highLevelSessionDispatcher
	val toMatch = RequestMatcher(request, request.path, RequestType(httpRequest), this)        
	if (sessionDispatch.isDefinedAt(toMatch)) {
	  runParams(request)
          try {
	  sessionDispatch(toMatch)(httpRequest) match {
	    case Full(r) => AnswerHolder(r)
	    case _ => AnswerHolder(request.createNotFound)
	  }
          } finally {
	  notices = Nil
          }
	} else {
          runParams(request)

          // make sure we're okay, sitemap wise
	  request.testLocation.foreach{s => S.error(s.msg); S.redirectTo(s.to)} 
	  
	  def idAndWhen(in: Node): Can[(String, String)] = 
	    ((in \ "@id").toList, in.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList) match {
	      case (x :: _, y :: _) => Full((x.text,y.value.text))
	      case _ => Empty
	    }
	  
	  findVisibleTemplate(request.path, request).map(xml => processSurroundAndInclude(request.uri, xml)) match {
	    case Full(xml: NodeSeq) => {
	      val realXml = allElems(xml, !_.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList.isEmpty) match {
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
	      AnswerHolder(XhtmlResponse(Group(request.fixHtml(realXml)), ResponseInfo.xhtmlTransitional, Nil, 200))
	    }
	    case _ => AnswerHolder(request.createNotFound)
	  }
	}
      } catch {
	case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[RedirectException]) =>
	  val rd = ite.getCause.asInstanceOf[RedirectException]
	notices = S.getNotices
	
	AnswerHolder(XhtmlResponse(Group(request.fixHtml(<html><body>{request.uri} Not Found</body></html>)),
					    ResponseInfo.xhtmlTransitional,
					    List("Location" -> (request.updateWithContextPath(rd.to))),
					    302))
	case rd : net.liftweb.http.RedirectException => {   
	  notices = S.getNotices
	  
	  AnswerHolder(XhtmlResponse(Group(request.fixHtml(<html><body>{request.uri} Not Found</body></html>)), ResponseInfo.xhtmlTransitional,
					      List("Location" -> (request.updateWithContextPath(rd.to))),
					      302))
	}
	case e  => AnswerHolder(LiftServlet.logAndReturnExceptionToBrowser(request, e)) // request.showException(e))
      }
    }
  }
  
  private def allElems(in: NodeSeq, f: Elem => Boolean): List[Elem] = {
    val lb = new ListBuffer[Elem]
    
    def appendAll(in: NodeSeq, lb: ListBuffer[Elem]) {
      in.foreach{
        case Group(ns) => appendAll(ns, lb)
        case e: Elem if f(e) => lb += e; appendAll(e.child, lb)
        case e: Elem => appendAll(e.child, lb)
        case _ => 
      }
    }
    appendAll(in, lb)
    
    lb.toList
  }
  
  private def findVisibleTemplate(path: ParsePath, session: RequestState) : Can[NodeSeq] = {
    val toMatch = RequestMatcher(session, session.path, session.requestType, this)
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
  
  private def findTemplate(name: String) : Can[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }
    
    findAnyTemplate(splits) or findAnyTemplate("templates-hidden" :: splits)
  }
  

  private val suffixes = List("", "html", "xhtml", "htm")
  
  private def locales: List[String] = {
    val locale = S.locale
    "_"+locale.toString :: "_"+locale.getLanguage :: "" :: Nil
  }
  
  def findAnyTemplate(places : List[String]) : Can[NodeSeq] = {
    val pls = places.mkString("/","/", "")
    val toTry = for (s <- suffixes; p <- locales) yield pls + p + (if (s.length > 0) "." + s else "")
    
    first(toTry)(v => LiftServlet.finder(v).flatMap(fc => PCDataXmlParser(fc))) or lookForClasses(places)
  }  
  
  private def lookForClasses(places : List[String]) : Can[NodeSeq] = {
    val controller: String = (places.take(1) orElse List("default_template"))(0)
    val action: String = (places.drop(1).take(1) orElse List("render"))(0)
    val trans = List((n:String) => n, (n:String) => smartCaps(n))
    val toTry = trans.flatMap(f => (LiftServlet.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "."+f(controller)))

    first(toTry) {
      clsName => 
	try {
	  tryo(List(classOf[ClassNotFoundException]), Empty) (Class.forName(clsName)).flatMap{
	    c =>
	      val inst = c.newInstance
	    c.getMethod(action, null).invoke(inst, null) match {
	      case null | Empty | None => Empty
              case n: Group => Full(n)
              case n: Elem => Full(n)
	      case s: NodeSeq => Full(s)
              case Some(n: Group) => Full(n)
              case Some(n: Elem) => Full(n)
              case Some(n: NodeSeq) => Full(n)
              case Some(n: Seq[Node]) => Full(n)
              case Full(n: Group) => Full(n)
              case Full(n: Elem) => Full(n)
	      case Full(n: NodeSeq) => Full(n)
	      case Full(n: Seq[Node]) => Full(n)
	      case _ => Empty
	    }
	  }
	} catch {
          case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[RedirectException]) => throw ite.getCause
          case re: RedirectException => throw re
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
  
  
  private def findAndEmbed(templateName: Can[Seq[Node]], kids : NodeSeq) : NodeSeq = {
    templateName match {
      case Full(tn) => {
	findTemplate(tn.text) match {
	  case Full(s) => processSurroundAndInclude(tn.text, s)
	  case _ => Comment("FIX"+"ME Unable to find template named "+tn.text) ++ kids
	}
      }
      case _ => Comment("FIX"+"ME No named specified for embedding") ++ kids
    }
  }
  
  private def findSnippetClass(name: String): Can[Class] = {
    if (name == null) Empty
    else findClass(name, LiftServlet.buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }
  
  private def findAttributeSnippet(name: String, rest: MetaData): MetaData = {
    val (cls, method) = splitColonPair(name, null, "render")
    findSnippetClass(cls).flatMap(clz => instantiate(clz).flatMap(inst => tryo(invokeMethod(clz, inst, method) match {
      case Full(md: MetaData) => Full(md.copy(rest))
      case _ => Empty
    }).flatMap(s => s))).openOr(rest)
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
  
  private val snippetClasses: HashMap[String, Class] = new HashMap
  
  private def findSnippetInstance(cls: String): Can[AnyRef] = S.snippetForClass(cls) or (findSnippetClass(cls).flatMap(c => instantiate(c)) match {
    case Full(inst: StatefulSnippet) => inst.snippetName = cls; S.setSnippetForClass(cls, inst); inst
    case Full(ret) => ret
    case _ => Empty
  })
  
  import LiftServlet.SnippetFailures._
  
  private def processSnippet(page: String, snippetName: Can[String], attrs: MetaData, kids: NodeSeq): NodeSeq = {
    val isForm = !attrs.get("form").toList.isEmpty
    val ret = snippetName.map(snippet => 
	S.locateSnippet(snippet).map(_(kids)) openOr {
	  val (cls, method) = splitColonPair(snippet, null, "render")
	  findSnippetInstance(cls) match {
            
            case Full(inst: StatefulSnippet) =>
            if (inst.dispatch.isDefinedAt(method))
            (if (isForm) S.hidden(ignore => inst.registerThisSnippet) else Text("")) ++ 
              inst.dispatch(method)(kids)
            else {LiftServlet.snippetFailedFunc.foreach(_(LiftServlet.SnippetFailure(page, snippetName, StatefulDispatchNotMatched))); kids}
              
	    case Full(inst) => {
              val ar: Array[Object] = List(Group(kids)).toArray
              // val ar: Array[Object] = Array(Group(kids))
	      ((invokeMethod(inst.getClass, inst, method, ar)) or invokeMethod(inst.getClass, inst, method)) match {
		case Full(md: NodeSeq) => md
		case it => LiftServlet.snippetFailedFunc.foreach(_(LiftServlet.SnippetFailure(page, snippetName, MethodNotFound))); kids
	      }
	    }
            case _ => LiftServlet.snippetFailedFunc.foreach(_(LiftServlet.SnippetFailure(page, snippetName, ClassNotFound))); kids
	  }
	}).openOr{
      LiftServlet.snippetFailedFunc.foreach(_(LiftServlet.SnippetFailure(page, snippetName, NoNameSpecified)))
      Comment("FIX"+"ME -- no type defined for snippet")
      kids
      }
    
    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", "multipart/form-data", Null)
    }
    
    attrs.get("form").map(ft => <form action={S.uri} method={ft.text}>{ret}</form> % checkMultiPart(attrs)) getOrElse ret
  }

      
  def fixHtml(in: NodeSeq): NodeSeq = RequestState.fixHtml(contextPath, in)
  
  private[http] def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq = {
    in.flatMap{
      v => 
	v match {
	  case Group(nodes) => Group(processSurroundAndInclude(page, nodes))
          case elm: Elem if elm.prefix == "lift" && elm.label == "ignore" => Text("")
          case elm: Elem if elm.prefix == "lift" && elm.label == "surround" => S.setVars(elm.attributes)(processSurroundElement(page, elm))
          case elm: Elem if elm.prefix == "lift" && elm.label == "embed" => S.setVars(elm.attributes)(processSurroundAndInclude(page, findAndEmbed(Can(elm.attributes.get("what")), elm.child)))
          case elm: Elem if elm.prefix == "lift" && elm.label == "snippet" => S.setVars(elm.attributes)(processSurroundAndInclude(page, processSnippet(page, Can(elm.attributes.get("type")).map(_.text), elm.attributes, elm.child)))
	  case elm: Elem if elm.prefix == "lift" && elm.label == "comet" => processSurroundAndInclude(page, executeComet(Can(elm.attributes.get("type").map(_.text.trim)), Can(elm.attributes.get("name").map(_.text.trim)), elm.child, elm.attributes))       
          case elm: Elem if elm.prefix == "lift" && elm.label == "children" => processSurroundAndInclude(page, elm.child)
          case elm: Elem if elm.prefix == "lift" && elm.label == "loc" => elm.attributes.filter(_.key == "id").toList match {case id :: _ => S.loc(id.value.text, elm.child) case _ => S.loc(elm.child.text, elm.child)}
          case elm: Elem if elm.prefix == "lift" && elm.label == "a" => Elem(null, "a", addAjaxHREF(elm.attributes), elm.scope, processSurroundAndInclude(page, elm.child): _*)
          case elm: Elem if elm.prefix == "lift" && elm.label == "form" => Elem(null, "form", addAjaxForm(elm.attributes), elm.scope, processSurroundAndInclude(page, elm.child): _*)
	  case elm: Elem => Elem(v.prefix, v.label, processAttributes(v.attributes), v.scope, processSurroundAndInclude(page, v.child) : _*)
	  case _ => v
	}
    }
  }
  
  private def executeComet(theType: Can[String], name: Can[String], kids: NodeSeq, attr: MetaData): NodeSeq = {
    try {
      findComet(theType, name, kids, Map.empty ++ attr.flatMap{case u: UnprefixedAttribute => List((u.key, u.value.text)) case u: PrefixedAttribute => List((u.pre+":"+u.key, u.value.text)) case _ => Nil}.toList).
      map(c =>
	(c !? (6600, AskRender)) match {
          case Some(AnswerRender(response, _, when, _)) if c.hasOuter => 
          <span id={c.uniqueId+"_outer"}>{c.buildSpan(when, response.inSpan)}{response.outSpan}</span>

          case Some(AnswerRender(response, _, when, _)) => 
          c.buildSpan(when, response.inSpan)
          
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
    findClass(contType, LiftServlet.buildPackage("comet") ::: ("lift.app.comet" :: Nil), {c : Class => classOf[CometActor].isAssignableFrom(c)}).flatMap{
      cls =>
	tryo((e: Throwable) => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)) {
	  val constr = cls.getConstructor(Array(this.getClass , classOf[Can[String]], classOf[NodeSeq], classOf[Map[String, String]]))
	  val ret = constr.newInstance(Array(this, name, defaultXml, attributes)).asInstanceOf[CometActor];
	  ret.start
	  // ret.link(this)
	  ret ! PerformSetupComet
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
    val pre = attr.filter(_.key == "onsubmit").toList match {
      case Nil => ""
      case x :: xs => x.value.text +";"
    }
    val ajax = "jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: jQuery('#"+id+"').serialize(), dataType: 'script', type: 'POST'}); "+pre+" return false;"
    new UnprefixedAttribute("id", id, new UnprefixedAttribute("action", "#", new UnprefixedAttribute("onsubmit", ajax, Null)))
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
  
  private def processSurroundElement(page: String, in: Elem): NodeSeq = {
    val attr = in.attributes
    val kids = in.child
    // case Elem("lift", "surround", attr @ _, _, kids @ _*) =>
      
      val (otherKids, paramElements) = filter2(kids) {
	case Elem("lift", "with-param", _, _, _) => false
	case _ => true
      }
    
    val params = paramElements.flatMap {
      case Elem("lift", "with-param", attr @ _, _, kids @ _*) =>
	val valueOption: Option[Seq[Node]] = attr.get("name")
      val option: Option[(String, NodeSeq)] = valueOption.map((v: Seq[Node]) => (v.text, processSurroundAndInclude(page, kids)))
      option
    }
    
    val mainParam = (attr.get("at").map(_.text: String).getOrElse("main"),
		     processSurroundAndInclude(page, otherKids))
      val paramsMap = collection.immutable.Map(params: _*) + mainParam
    findAndMerge(attr.get("with"), paramsMap)
  }
  
  private def findAndMerge(templateName: Can[Seq[Node]], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name = templateName.map(s => if (s.text.startsWith("/")) s.text else "/"+ s.text).openOr("/templates-hidden/default")
    
    findTemplate(name).map(s => processBind(processSurroundAndInclude(name, s), atWhat)).
      openOr(atWhat.values.flatMap(_.elements).toList)
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

// case class AskSessionToRender(request: RequestState,httpRequest: HttpServletRequest,timeout: Long, sendBack: AnswerHolder => Any)
// case class UpdateState(name: String, value: Can[String]) extends SessionMessage
// case object CurrentVars extends SessionMessage
case object ShutDown

case class AnswerHolder(what: ResponseIt)

// vim: set ts=2 sw=2 et:
