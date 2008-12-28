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

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._
import _root_.scala.reflect.Manifest
import _root_.javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent, HttpSession}
import _root_.scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import _root_.scala.xml.{NodeSeq, Unparsed, Text}
import _root_.net.liftweb.util._
import Box._
import _root_.net.liftweb.http.js.{JsCmd, AjaxInfo}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.builtin.snippet._
import _root_.java.lang.reflect.{Method, Modifier, InvocationTargetException}
import _root_.scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import _root_.java.io.InputStream
import _root_.javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
import _root_.scala.xml.transform._
import _root_.java.util.concurrent.TimeUnit
import js._

object LiftSession {

  /**
   * Returns a reference to a LiftSession dictated by LiftRules#sessionCreator function.
   */
  def apply(session: HttpSession, contextPath: String, headers: List[(String, String)]) =
  LiftRules.sessionCreator(session, contextPath, headers)

  var onSessionActivate: List[LiftSession => Unit] = Nil
  var onSessionPassivate: List[LiftSession => Unit] = Nil
  var onSetupSession: List[LiftSession => Unit] = Nil
  var onAboutToShutdownSession: List[LiftSession => Unit] = Nil
  var onShutdownSession: List[LiftSession => Unit] = Nil
  var onBeginServicing: List[(LiftSession, Req) => Unit] = Nil
  var onEndServicing: List[(LiftSession, Req, Box[LiftResponse]) => Unit] = Nil
}


private[http] case class AddSession(session: LiftSession)
private[http] case class RemoveSession(sessionId: String)
case class SessionWatcherInfo(sessions: Map[String, LiftSession])

@serializable
case class SessionToServletBridge(uniqueId: String) extends HttpSessionBindingListener with HttpSessionActivationListener {
  def sessionDidActivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
      LiftSession.onSessionActivate.foreach(_(ls)))
  }

  def sessionWillPassivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId, Empty).foreach(ls =>
      LiftSession.onSessionPassivate.foreach(_(ls)))
  }

  def valueBound(event: HttpSessionBindingEvent) {

  }


  /**
   * When the session is unbound the the HTTP session, stop us
   */
  def valueUnbound(event: HttpSessionBindingEvent) {
    SessionMaster ! RemoveSession(uniqueId)

  }

}

/**
 * Manages LiftSessions because the servlet container is less than optimal at
 * timing sessions out.
 */
object SessionMaster extends Actor {
  private case class LookupSession(uniqueId: String, when: Long)

  private var sessions: Map[String, LiftSession] = Map.empty
  private object CheckAndPurge

  def getSession(id: String, otherId: Box[String]): Box[LiftSession] = synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(id))
  }

  /**
   * Put an Actor in this list and the Actor will receive a message
   * every 10 seconds with the current list of sessions:
   * SessionWatcherInfo
   */
  var sessionWatchers: List[Actor] = Nil

  def getSession(httpSession: HttpSession, otherId: Box[String]): Box[LiftSession] =
  synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(httpSession.getId()))
  }

  def getSession(req: HttpServletRequest, otherId: Box[String]): Box[LiftSession] =
  synchronized {
    otherId.flatMap(sessions.get) or Box(sessions.get(req.getSession.getId()))
  }

  def addSession(liftSession: LiftSession) {
    synchronized {
      sessions = sessions + (liftSession.uniqueId -> liftSession)
    }
    liftSession.startSession()
    val b = SessionToServletBridge(liftSession.uniqueId)
    liftSession.httpSession.setAttribute(LiftMagicID, b)
  }

  private val LiftMagicID = "$lift_magic_session_thingy$"

  def act = {
    doPing()
    link(ActorWatcher)
    loop {
      react {
        case RemoveSession(sessionId) =>
          val ses = synchronized(sessions)
          ses.get(sessionId).foreach{s =>
            try {
              s.doShutDown
              try {
                s.httpSession.removeAttribute(LiftMagicID)
              } catch {
                case e => // ignore... sometimes you can't do this and it's okay
              }
            } catch {
              case e => Log.error("Failure in remove session", e)

            } finally {
              synchronized{ sessions = sessions - sessionId }
            }
          }

        case CheckAndPurge =>
          val now = millis
          val ses = synchronized{sessions}
          for ((id, session) <- ses.elements) {
            if (now - session.lastServiceTime > session.inactivityLength) {
              Log.info(" Session "+id+" expired")
              this ! RemoveSession(id)
            }
          }
          sessionWatchers.foreach(_ ! SessionWatcherInfo(ses))
          doPing()
      }
    }
  }

  this.start

  private def doPing() {
    try {
      ActorPing schedule(this, CheckAndPurge, 10 seconds)
    } catch {
      case e => Log.error("Couldn't start SessionMaster ping", e)
    }
  }


}

@serializable
class LiftSession(val contextPath: String, val uniqueId: String,
                  val httpSession: HttpSession, val initialHeaders: List[(String, String)]) {
  import TemplateFinder._

  private var running_? = false

  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap

  private[http] var notices: Seq[(NoticeType.Value, NodeSeq, Box[String])] = Nil

  private var asyncComponents = new HashMap[(Box[String], Box[String]), CometActor]()

  private var asyncById = new HashMap[String, CometActor]()

  private var myVariables: Map[String, Any] = Map.empty

  private var onSessionEnd: List[LiftSession => Unit] = Nil

  @volatile
  private[http] var lastServiceTime = millis

  @volatile
  private[http] var inactivityLength = 180000L

  private [http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPF]()
  private [http] var sessionRewriter = new HashMap[String, LiftRules.RewritePF]()

  private[http] def startSession(): Unit = {
    running_? = true
    inactivityLength = httpSession.getMaxInactiveInterval.toLong * 1000L
    lastServiceTime = millis
    LiftSession.onSetupSession.foreach(_(this))
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

  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Box[String])

  def runParams(state: Req): List[Any] = {
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
      case bfh: S.BinFuncHolder =>
        () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
      case normal =>
        () => normal(state.params.getOrElse(i.name,
                                            state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }

    val ret = toRun.map(_.owner).removeDuplicates.flatMap{w =>
      val f = toRun.filter(_.owner == w);
      w match {
        // if it's going to a CometActor, batch up the commands
        case Full(id) =>
          asyncById.get(id).toList.
          flatMap(a => a !? ActionMessageSet(f.map(i => buildFunc(i)), state) match
                  {
              case Some(li: List[_]) => li
              case li: List[_] => li
              case other => Nil})

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
   * Called just before the session exits.  If there's clean-up work, override this method
   */
  def cleanUpSession() {
    messageCallback = HashMap.empty
    notices = Nil
    asyncComponents.clear
    asyncById = HashMap.empty
    myVariables = Map.empty
    onSessionEnd = Nil
    highLevelSessionDispatcher = HashMap.empty
    sessionRewriter = HashMap.empty
  }

  def addSessionCleanup(f: LiftSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  private[http] def doShutDown() {
    try {
      if (running_?) this.shutDown()
    } finally {
      Actor.clearSelf
    }
  }

  private def shutDown() = synchronized {
    onSessionEnd.foreach(_(this))

    LiftSession.onAboutToShutdownSession.foreach(_(this))
    SessionMaster ! RemoveSession(this.uniqueId)

    // Log.debug("Shutting down session")
    running_? = false
    asyncComponents.foreach{case (_, comp) => comp ! ShutDown}
    cleanUpSession()
    LiftSession.onShutdownSession.foreach(_(this))
  }

  /**
   * Find the template assocaited with the Loc
   */
  private[http] def locTemplate: Box[NodeSeq] =
  for (req <- S.request;
       loc <- req.location;
       template <- loc.template) yield template

  private[http] def processRequest(request: Req): Box[LiftResponse] = {
    S.oldNotices(notices)
    LiftSession.onBeginServicing.foreach(f => tryo(f(this, request)))
    val ret = try {
      val sessionDispatch = S.highLevelSessionDispatcher

      val toMatch = request
      NamedPF.applyBox(toMatch, sessionDispatch) match {
        case Full(f) =>
          runParams(request)
          try {
            f() match {
              case Full(r) => Full(checkRedirect(r))
              case _ => LiftRules.notFoundOrIgnore(request, Full(this))
            }
          } finally {
            notices = S.getNotices
          }

        case _ =>
          runParams(request)

          def idAndWhen(in: Node): Box[CometVersionPair] =
          ((in \ "@id").toList, in.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList) match {
            case (x :: _, y :: _) => Full(CVP(x.text,toLong(y.value.text)))
            case _ => Empty
          }

          // Process but make sure we're okay, sitemap wise
          val response: Box[LiftResponse] = request.testLocation match {
            case Left(true) =>
              cleanUpBeforeRender
              ((locTemplate or findVisibleTemplate(request.path, request)).
               map(xml => processSurroundAndInclude(request.uri+" -> "+request.path, xml)) match {
                  case Full(rawXml: NodeSeq) => {

                    val xml = HeadHelper.mergeToHtmlHead(rawXml)
                    val cometXform: List[RewriteRule] =
                    if (LiftRules.autoIncludeComet(this))
                      allElems(xml, !_.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList.isEmpty) match {
                        case Nil => Nil
                        case xs =>
                          val comets: List[CometVersionPair] = xs.flatMap(x => idAndWhen(x))
                          new AddScriptToBody(comets) :: Nil
                      }
                    else Nil

                    val ajaxXform: List[RewriteRule] =
                    if (LiftRules.autoIncludeAjax(this)) new AddAjaxToBody() :: cometXform
                      else cometXform


                    val realXml = if (ajaxXform.isEmpty) xml
                      else (new RuleTransformer(ajaxXform :_*)).transform(xml)

                    this.synchronized {
                      S.functionMap.foreach(mi => messageCallback(mi._1) = mi._2)
                    }
                    notices = Nil // S.getNotices
                    Full(LiftRules.convertResponse((realXml,
                                                    S.getHeaders(LiftRules.defaultHeaders((realXml, request))),
                                                    S.responseCookies,
                                                    request)))
                  }
                  case _ => if (LiftRules.passNotFoundToChain) Empty else Full(request.createNotFound)
                })
            case Right(msg) => msg
            case _ => if (LiftRules.passNotFoundToChain) Empty else Full(request.createNotFound)
          }

          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect)
      }
    } catch {
      case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))

      case rd: _root_.net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))

      case e => NamedPF.applyBox((Props.mode, request, e), LiftRules.exceptionHandler.toList);

    }

    LiftSession.onEndServicing.foreach(f => tryo(f(this, request, ret)))
    ret
  }

  private def cleanUpBeforeRender {
    // Reset the mapping between ID and Style for Ajax notices.
	MsgErrorMeta(new HashMap)
    MsgWarningMeta(new HashMap)
    MsgNoticeMeta(new HashMap)
  }

  private def handleRedirect(re: ResponseShortcutException, request: Req): LiftResponse = {
    if (re.doNotices) notices = S.getNotices

    re.response
  }

  /**
   * Set a session-local variable to a value
   *
   * @param name -- the name of the variable
   * @param value -- the value of the variable
   */
  private [liftweb] def set[T](name: String, value: T): Unit = synchronized {
    myVariables = myVariables + (name -> value)
  }

  /**
   * Gets the named variable if it exists
   *
   * @param name -- the name of the session-local variable to get
   *
   * @return Full(value) if found, Empty otherwise
   */
  private [liftweb] def get[T](name: String): Box[T] = synchronized {
    Box(myVariables.get(name)).asInstanceOf[Box[T]]
  }

  /**
   * Unset the named variable
   *
   * @param name the variable to unset
   */
  private [liftweb] def unset(name: String): Unit = synchronized {
    myVariables -= name
  }

  private[http] def attachRedirectFunc(uri: String, f : Box[() => Unit]) = {
    f map { fnc =>
      val func: String = LiftSession.this.synchronized {
        val funcName = Helpers.nextFuncName
        messageCallback(funcName) = S.NFuncHolder(() => {
            try {
              fnc()
            } finally {
              LiftSession.this.synchronized {
                messageCallback -= funcName
              }
            }
          })
        funcName
      }
      val sep = uri contains("?") match {case true => "&" case _ => "?"}
      uri + sep + func +"=_"
    } openOr uri

  }

  private[http] def checkRedirect(resp: LiftResponse): LiftResponse = resp match {
    case RedirectWithState(uri, state, cookies @ _*) =>
      state.msgs.foreach(m => S.message(m._1, m._2))
      notices = S.getNotices
      RedirectResponse(attachRedirectFunc(uri, state.func), cookies:_*)
    case _ => resp
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

private def findVisibleTemplate(path: ParsePath, session: Req): Box[NodeSeq] = {
  val tpath = path.partPath
  val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
    case s @ _ if (!s.isEmpty) => s
    case _ => List("index")
  }
  findAnyTemplate(splits)
}

  private def findTemplate(name: String): Box[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }

     findAnyTemplate("templates-hidden" :: splits) or findAnyTemplate(splits)
  }

  def couldBeHtml(in : List[(String, String)]) : Boolean = {
    in match {
      case null | Nil => true
      case _ => in.ciGet("Content-Type").map(_.toLowerCase.contains("html")) openOr true
    }
  }


  private def findAndEmbed(templateName: Box[Seq[Node]], kids: NodeSeq): NodeSeq = {
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

  private def findSnippetClass(name: String): Box[Class[AnyRef]] = {
    if (name == null) Empty
    else findClass(name, LiftRules.buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil))
  }

  private def findAttributeSnippet(name: String, rest: MetaData): MetaData = {
    val (cls, method) = splitColonPair(name, null, "render")

    findSnippetClass(cls).flatMap(clz => instantiate(clz).flatMap(inst =>
        (invokeMethod(clz, inst, method) match {
            case Full(md: MetaData) => Full(md.copy(rest))
            case _ => Empty
          }))).openOr(rest)
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


  private def findSnippetInstance(cls: String): Box[AnyRef] =
  S.snippetForClass(cls) or
  (findSnippetClass(cls).flatMap(c => instantiate(c)) match {
      case Full(inst: StatefulSnippet) =>
        inst.snippetName = cls; S.setSnippetForClass(cls, inst); Full(inst)
      case Full(ret) => Full(ret)
      case _ => Empty
    })


  private def processSnippet(page: String, snippetName: Box[String], attrs: MetaData, passedKids: NodeSeq): NodeSeq = {
    val isForm = !attrs.get("form").toList.isEmpty

    val eagerEval: Boolean = attrs.get("eager_eval").map(toBoolean) getOrElse false

    val kids = if (eagerEval) processSurroundAndInclude(page, passedKids) else passedKids

    def locSnippet(snippet: String): Box[NodeSeq] =
    for (req <- S.request;
         loc <- req.location;
         func <- loc.snippet(snippet)) yield func(kids)

    val ret: NodeSeq = snippetName.map(snippet =>
      S.doSnippet(snippet)(
        (S.locateMappedSnippet(snippet).map(_(kids)) or
         locSnippet(snippet)).openOr(
          S.locateSnippet(snippet).map(_(kids)) openOr {
            val (cls, method) = splitColonPair(snippet, null, "render")
            (LiftRules.snippet(cls) or
             findSnippetInstance(cls)) match {

              case Full(inst: StatefulSnippet) =>
                if (inst.dispatch.isDefinedAt(method))
                (if (isForm) SHtml.hidden(() => inst.registerThisSnippet) else Text("")) ++
                inst.dispatch(method)(kids)
                else {LiftRules.snippetFailedFunc.toList.foreach(_(LiftRules.SnippetFailure(page, snippetName,
                                                                                     LiftRules.SnippetFailures.StatefulDispatchNotMatched))); kids}
              case Full(inst: DispatchSnippet) =>
                if (inst.dispatch.isDefinedAt(method)) inst.dispatch(method)(kids)
                else {LiftRules.snippetFailedFunc.toList.foreach(_(LiftRules.SnippetFailure(page, snippetName,
                                                                                     LiftRules.SnippetFailures.StatefulDispatchNotMatched))); kids}

              case Full(inst) => {
                  val ar: Array[Object] = List(Group(kids)).toArray
                  ((invokeMethod(inst.getClass, inst, method, ar)) or invokeMethod(inst.getClass, inst, method)) match {
                    case Full(md: NodeSeq) => md
                    case it => LiftRules.snippetFailedFunc.toList.foreach(_(LiftRules.SnippetFailure(page, snippetName,
                                                                                              LiftRules.SnippetFailures.MethodNotFound))); kids
                  }
                }
              case _ => LiftRules.snippetFailedFunc.toList.foreach(_(LiftRules.SnippetFailure(page, snippetName,
                                                                                       LiftRules.SnippetFailures.ClassNotFound))); kids
            }
          }))).openOr{
      LiftRules.snippetFailedFunc.toList.foreach(_(LiftRules.SnippetFailure(page, snippetName,
                                                                     LiftRules.SnippetFailures.NoNameSpecified)))
      Comment("FIX"+"ME -- no type defined for snippet")
      kids
    }

    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null)
    }

    def checkAttr(attr_name: String, in: MetaData): MetaData =
    in.filter(_.key == attr_name).toList match {
      case Nil => Null
      case x => new UnprefixedAttribute(attr_name, Text(x.first.value.text),
                                        Null)
    }

    if (ret.isEmpty) ret else
    attrs.get("form").map(ft => (
        (<form action={S.uri} method={ft.text.trim.toLowerCase}>{ret}</form> %
         checkMultiPart(attrs)) %
        checkAttr("class", attrs)) % checkAttr("id",attrs) ) getOrElse ret

  }


  def fixHtml(in: NodeSeq): NodeSeq = Req.fixHtml(contextPath, in)


  /**
   * The partial function that defines how lift tags are processed for this session.  Initially composed
   * of LiftRules.liftTagProcessing orElse the default lift tag processing.  If you need to change the
   * way a particular session handles lift tags, alter this partial function.
   */
  var liftTagProcessing: List[LiftRules.LiftTagPF] = _

  /**
   * The basic partial function that does lift tag processing
   */
  private def _defaultLiftTagProcessing: LiftRules.LiftTagPF =
  NamedPF("Default Lift Tags") {
    case ("snippet", elm, metaData, kids, page) =>
      metaData.get("type") match {
        case Some(tn) => NamedPF((tn.text, elm, metaData, kids, page),
                                 liftTagProcessing)
        case _ => processSnippet(page, Empty , elm.attributes, elm.child)
      }
    case ("surround", elm, _, _, page) => processSurroundElement(page, elm)
    case ("embed", _, metaData, kids, page) => findAndEmbed(Box(metaData.get("what")), kids)
    case ("ignore", _, _, _, _) => Text("")
    case ("comet", _, metaData, kids, _) => executeComet(Box(metaData.get("type").map(_.text.trim)), Box(metaData.get("name").map(_.text.trim)), kids, metaData)
    case ("children", _, _, kids, _) => kids
    case ("a", elm, metaData, kids, _) => Elem(null, "a", addAjaxHREF(metaData), elm.scope, kids :_*)
    case ("form", elm, metaData, kids, _) => Elem(null, "form", addAjaxForm(metaData), elm.scope, kids : _*)
    case ("loc", elm, metaData, kids, _) => metaData.get("locid") match {case Some(id) => S.loc(id.text, kids) case _ => S.loc(kids.text, kids)}
    case (snippetInfo, elm, metaData, kids, page) => processSnippet(page, Full(snippetInfo) , metaData, kids)
  }

  liftTagProcessing = LiftRules.liftTagProcessing.toList ::: List(_defaultLiftTagProcessing)

  private def asNodeSeq(in: Seq[Node]): NodeSeq = in

  def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq = {
    in.flatMap{
      v =>
      v match {
        case Group(nodes) => Group(processSurroundAndInclude(page, nodes))
        case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" => S.setVars(elm.attributes)(processSurroundAndInclude(page, NamedPF((elm.label, elm, elm.attributes, asNodeSeq(elm.child), page), liftTagProcessing)))
        case elm: Elem => Elem(v.prefix, v.label, processAttributes(v.attributes), v.scope, processSurroundAndInclude(page, v.child) : _*)
        case _ => v
      }
    }
  }

  private def executeComet(theType: Box[String], name: Box[String], kids: NodeSeq, attr: MetaData): NodeSeq = {
    try {
      findComet(theType, name, kids, Map.empty ++
                attr.flatMap{
          case u: UnprefixedAttribute => List((u.key, u.value.text))
          case u: PrefixedAttribute => List((u.pre+":"+u.key, u.value.text))
          case _ => Nil}.toList).
      map(c =>
        (c !? (26600, AskRender)) match {
          case Some(AnswerRender(response, _, when, _)) if c.hasOuter =>
            <span id={c.uniqueId+"_outer"}>{c.buildSpan(when, response.inSpan)}{response.outSpan}</span>

          case Some(AnswerRender(response, _, when, _)) =>
            c.buildSpan(when, response.inSpan)

          case _ => <span id={c.uniqueId} lift:when="0">{Comment("FIX"+"ME comet type "+theType+" name "+name+" timeout") ++ kids}</span>
        }) openOr Comment("FIX"+"ME - comet type: "+theType+" name: "+name+" Not Found ") ++ kids
    } catch {
      case e => Log.error("Failed to find a comet actor", e); kids
    }
  }

  def findComet(theType: String): List[CometActor] = synchronized {
    asyncComponents.elements.filter{case ((Full(name), _), _) => name == theType case _ => false}.toList.map{case (_, value) => value}
  }

  private def findComet(theType: Box[String], name: Box[String], defaultXml: NodeSeq, attributes: Map[String, String]): Box[CometActor] = {
    val what = (theType, name)
    Box(asyncComponents.get(what)).or( {
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

  def getAsyncComponent(id: String): Box[CometActor] = synchronized(asyncById.get(id))

  def addCometActor(act: CometActor): Unit = synchronized {
    asyncById(act.uniqueId) = act
  }

  def removeCometActor(act: CometActor): Unit = synchronized {
    asyncById -= act.uniqueId
    messageCallback -= act.jsonCall.funcId

    // FIXME remove all the stuff from the function table related to this item

  }

  private def findCometByType(contType: String, name: Box[String], defaultXml: NodeSeq, attributes: Map[String, String]): Box[CometActor] = {
    findType[CometActor](contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil)).flatMap{
      cls =>
      tryo((e: Throwable) => e match {case e: _root_.java.lang.NoSuchMethodException => ()
          case e => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)}) {
        val constr = cls.getConstructor()
        val ret = constr.newInstance().asInstanceOf[CometActor]
        ret.initCometActor(this, name, defaultXml, attributes)

        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }  or tryo((e: Throwable) => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)) {
        val constr = cls.getConstructor(this.getClass , classOf[Box[String]], classOf[NodeSeq], classOf[Map[String, String]])
        val ret = constr.newInstance(this, name, defaultXml, attributes).asInstanceOf[CometActor];
        ret.start
        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }
    }
  }



  private def addAjaxHREF(attr: MetaData): MetaData = {
    val ajax: JsExp = SHtml.makeAjaxCall(JE.Str(attr("key")+"=true"))

    new UnprefixedAttribute("onclick", Text(ajax.toJsCmd), new UnprefixedAttribute("href", Text("javascript://"), attr.filter(a => a.key != "onclick" && a.key != "href")))
  }

  private def addAjaxForm(attr: MetaData): MetaData = {
    val id = Helpers.nextFuncName
    val pre = attr.filter(_.key == "onsubmit").toList match {
      case Nil => ""
      case x :: xs => x.value.text +";"
    }

    val ajax: String =
    SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(id)).toJsCmd + ";" +
    pre + "return false;"

    // val ajax = LiftRules.jsArtifacts.ajax(AjaxInfo(LiftRules.jsArtifacts.serialize(id).toJsCmd, true)) + pre + " return false;"

    new UnprefixedAttribute("id", Text(id), new UnprefixedAttribute("action", Text("#"), new UnprefixedAttribute("onsubmit", Text(ajax), attr.filter(a => a.key != "id" && a.key != "onsubmit" && a.key != "action"))))
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

  private def findAndMerge(templateName: Box[Seq[Node]], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name = templateName.map(s => if (s.text.startsWith("/")) s.text else "/"+ s.text).openOr("/templates-hidden/default")

    findTemplate(name).map(s => processBind(s, atWhat)).
    openOr(atWhat.values.flatMap(_.elements).toList)
  }

  class AddAjaxToBody() extends RewriteRule {
    private var done = false
    override def transform(n: Node) = n match {
      case e: Elem if e.label == "head" && !done =>
        done = true
        Elem(null, "head", e.attributes,  e.scope, (e.child ++
                                                    <script
              src={S.encodeURL("/"+
                               LiftRules.ajaxPath +
                               "/" + LiftRules.ajaxScriptName())}
              type="text/javascript"/>) :_*)
      case n => n
    }
  }

  class AddScriptToBody(val cometVar: List[CometVersionPair]) extends RewriteRule {
    private var doneHead = false
    private var doneBody = false

    override def transform(n: Node) = n match {
      case e: Elem if e.label == "head" && !doneHead =>
        doneHead = true
        Elem(null, "head",
             e.attributes,
             e.scope,
             (e.child ++
              <script src={S.encodeURL("/"+
                                       LiftRules.cometPath +
                                       "/" + uniqueId +
                                       "/" + LiftRules.cometScriptName())}
              type="text/javascript"/>) :_*)

      case e: Elem if e.label == "body" && !doneBody =>
        doneBody = true
        Elem(null, "body", e.attributes,  e.scope, (e.child ++
                                                    JsCmds.Script(LiftRules.renderCometPageContents(LiftSession.this, cometVar))) :_*)
      case n => n
    }
  }
}

/**
 * The response from a page saying that it's been rendered
 */
case object ShutDown

/**
 * If a class is to be used as a lift view (rendering from code rather than a static template)
 * and the method names are to be used as "actions", the view must be marked as "InsecureLiftView"
 * because there exists the ability to execute arbitrary methods based on wire content
 */
trait InsecureLiftView

/**
 *  The preferred way to do lift views... implement a partial function that dispatches
 * the incoming request to an appropriate method
 */
trait LiftView {
  implicit def nsToCns(in: NodeSeq): Box[NodeSeq] = Box.legacyNullTest(in)
  def dispatch : PartialFunction[String, () => Box[NodeSeq]]
}

object TemplateFinder {
  private val suffixes = List("", "html", "xhtml", "htm")

  import LiftRules.ViewDispatchPF

  private def checkForLiftView(part: List[String], last: String, what: ViewDispatchPF): Box[NodeSeq] = {
    if (what.isDefinedAt(part)) {
      what(part) match {
        case Right(lv) => if (lv.dispatch.isDefinedAt(last)) lv.dispatch(last)() else Empty
        case _ => Empty
      }
    } else Empty
  }

  private def checkForFunc(whole: List[String], what: ViewDispatchPF): Box[NodeSeq] =
    if (what.isDefinedAt(whole)) what(whole) match {
      case Left(func) => func()
      case _ => Empty
    }
    else Empty

  private def findInViews(whole: List[String], part: List[String],
                          last: String,
                          what: List[ViewDispatchPF]): Box[NodeSeq] =
  what match {
    case Nil => Empty
    case x :: xs =>
      (checkForLiftView(part, last, x) or checkForFunc(whole, x)) match {
        case Full(ret) => Full(ret)
        case _ => findInViews(whole, part, last, xs)
      }
  }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String]): Box[NodeSeq] = {
    val part = places.dropRight(1)
    val last = places.last

    findInViews(places, part, last, LiftRules.viewDispatch.toList) match {
      case Full(lv) =>
        Full(lv)

      case _ =>
        val pls = places.mkString("/","/", "")
        val toTry = for (s <- suffixes; p <- locales) yield pls + p + (if (s.length > 0) "." + s else "")

        first(toTry)(v => LiftRules.finder(v).flatMap(fc => PCDataXmlParser(fc))) or lookForClasses(places)
    }
  }

  private def locales: List[String] = {
    val locale = S.locale
    "_"+locale.toString :: "_"+locale.getLanguage :: "" :: Nil
  }


  private def lookForClasses(places : List[String]): Box[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelCase(n))
    val toTry = trans.flatMap(f => (LiftRules.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "."+f(controller)))

    first(toTry) {
      clsName =>
      try {
        tryo(List(classOf[ClassNotFoundException]), Empty) (Class.forName(clsName).asInstanceOf[Class[AnyRef]]).flatMap{
          c =>
          (c.newInstance match {
              case inst: InsecureLiftView => c.getMethod(action).invoke(inst)
              case inst: LiftView if inst.dispatch.isDefinedAt(action) => inst.dispatch(action)()
              case _ => Empty
            }) match {
            case null | Empty | None => Empty
            case n: Group => Full(n)
            case n: Elem => Full(n)
            case s: NodeSeq => Full(s)
            case Some(n: Group) => Full(n)
            case Some(n: Elem) => Full(n)
            case Some(n: NodeSeq) => Full(n)
            case Some(SafeNodeSeq(n)) => Full(n)
            case Full(n: Group) => Full(n)
            case Full(n: Elem) => Full(n)
            case Full(n: NodeSeq) => Full(n)
            case Full(SafeNodeSeq(n)) => Full(n)
            case _ => Empty
          }
        }
      } catch {
        case ite: _root_.java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) => throw ite.getCause
        case re: ResponseShortcutException => throw re
        case _ => Empty
      }
    }
  }
}

