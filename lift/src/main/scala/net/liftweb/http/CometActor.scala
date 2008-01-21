package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.{Actor}
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import net.liftweb.util.{Helpers, Log, Can, Full, Empty, Failure}
import scala.xml.{NodeSeq, Text, Elem, Unparsed, Node, Group, Null, PrefixedAttribute}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{HashSet, ListBuffer}
// import S._
import net.liftweb.http.js._
import JsCmds._

// import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent}

@serializable 
abstract class CometActor(val theSession: LiftSession, val name: Can[String], val defaultXml: NodeSeq, val attributes: Map[String, String]) extends Actor {
  
  @serializable
  private object Never
  val uniqueId = "LC"+randomString(20)
  private var lastRenderTime = millis
  private var lastRendering: RenderOut = RenderOut(Full(defaultXml),
						   Empty, Empty, Empty, false)
  private var wasLastFullRender = false
  @transient
  private var t_listeners = new ListBuffer[Actor]()
  private var askingWho: Can[CometActor] = Empty
  private var whosAsking: Can[CometActor] = Empty
  private var answerWith: Can[Any => Any] = Empty
  private var deltas: List[Delta] = Nil
  
  private def listeners = {
    if (t_listeners == null) t_listeners = new ListBuffer
    t_listeners
  }
  
  def defaultPrefix: String
  
  /**
    * Set to 'true' if we should run "render" on every page load
    */
  protected def devMode = false
  
  def hasOuter = true
  
  def parentTag = <span/>

  def handleJSON(in: Any): JsCmd = Noop

  lazy val (jsonCall, jsonInCode) = S.buildJSONFunc(handleJSON)
  
  def buildSpan(time: Long, xml: NodeSeq): NodeSeq = Elem(parentTag.prefix, parentTag.label, parentTag.attributes, 
      parentTag.scope, Group(xml)) % ("id" -> uniqueId) % (new PrefixedAttribute("lift", "when", time.toString, Null))
          
  
  def act = {
    this.trapExit = true
    loop {
    react(composeFunction)
    }
  }
  
  def fixedRender: Can[NodeSeq] = Empty

  def highPriority : PartialFunction[Any, Unit] = {
    case Never =>
  }
  
  def lowPriority : PartialFunction[Any, Unit] = {
  case Never =>
}
  
  def mediumPriority : PartialFunction[Any, Unit] = {
  case Never =>
}  
  
  private def _lowPriority : PartialFunction[Any, Unit] = {
    case s => Log.debug("CometActor "+this+" got unexpected message "+s)
  }
  
  private def _mediumPriority : PartialFunction[Any, Unit] = {
    case Unlisten => listeners -= sender.receiver
    
    case l @ Listen(when) =>
    askingWho match {
      case Full(who) => who forward l
      case _ =>
        deltas.filter(_.when > when) match { 
          case Nil => if (when >= lastRenderTime) listeners += sender.receiver
        else sender.receiver ! AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _), whosAsking openOr this, lastRenderTime, wasLastFullRender)
          case all @ (hd :: xs) => sender.receiver ! AnswerRender(new XmlOrJsCmd(uniqueId, Empty, Empty, 
              Full(all.reverse.foldLeft(Noop.asInstanceOf[JsCmd])(_ ++ _.js)), Empty, buildSpan, false), whosAsking openOr this, hd.when, false)
        }
    }
    
    case PerformSetupComet =>
      localSetup
      reRender(true)
    
    case AskRender =>
      askingWho match {
        case Full(who) => who forward AskRender
        case _ => if (!deltas.isEmpty || devMode) reRender(false); reply(AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _), whosAsking openOr this, lastRenderTime, true))
        }
    
    case ActionMessageSet(msgs, request) =>
    S.init(request, theSession) {
      val ret = msgs.map(_())
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      reply(ret)
    }    
      
    case AskQuestion(what, who) =>
      startQuestion(what)
      whosAsking = Full(who)
    
    case AnswerQuestion(what, request) =>
      S.init(request, theSession) {
        askingWho.foreach {
          askingWho =>
        reply("A null message to release the actor from its send and await reply... do not delete this message")
	askingWho.unlink(self)
        askingWho ! ShutDown
	this.askingWho = Empty
        val aw = answerWith
        answerWith = Empty
	aw.foreach(_(what))
        reRender(true)
        }
      }
      
    case ReRender(all) => reRender(all)

    case ShutDown =>
    theSession.removeCometActor(this)
    localShutdown()
    self.exit("Politely Asked to Exit")
  }
  /**
   * It's the main method to override, to define what is rendered by the CometActor
   *
   * There are implicit conversions for a bunch of stuff to
   * RenderOut (including NodeSeq).  Thus, if you don't declare the return
   * turn to be something other than RenderOut and return something that's
   * coersable into RenderOut, the compiler "does the right thing"(tm) for you.
   */
  def render: RenderOut
  
  def compute: Map[String, Any] = Map.empty[String, Any]
  
  final def reRender(sendAll: Boolean): AnswerRender = {
    lastRenderTime = Math.max(millis, lastRenderTime + 1)
    wasLastFullRender = sendAll & hasOuter
    deltas = Nil
    S.initIfUninitted(theSession) {
      lastRendering = render
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      val rendered: AnswerRender = AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _), this, lastRenderTime, sendAll)// buildRendered(lastRendering, lastRenderTime)
      listeners.toList.foreach(_ ! rendered)
      listeners.clear
      rendered
    }
  }
  
  protected def partialUpdate(cmd: JsCmd) {
    val time = deltas match {
      case Nil => millis + 1
      case hd :: _ => hd.when.max( millis + 1)
    }
    val delta = JsDelta(time, cmd)
    val garbageTime = time - 120000L // remove anything that's more than 2 minutes old
    deltas = delta :: deltas.filter(_.when < garbageTime)
    if (!listeners.isEmpty) {
      val rendered = AnswerRender(new XmlOrJsCmd(uniqueId, Empty, Empty, 
          Full(cmd), Empty, buildSpan, false), whosAsking openOr this, time, false)
      listeners.toList.foreach(_ ! rendered)
      listeners.clear
    }
  }
  
  def startQuestion(what: Any) {}
  
  /**
    * This method will be called after the Actor has started.  Do any setup here
    */
  def localSetup(): Unit = {}
  
  /**
    * This method will be called as part of the shut-down of the actor.  Release any resources here.
    */
  def localShutdown(): Unit = {}
  
  def composeFunction = composeFunction_i
  
  def composeFunction_i = highPriority orElse mediumPriority orElse _mediumPriority orElse lowPriority orElse _lowPriority
  
  def bind(prefix: String, vals: (String, NodeSeq)*): NodeSeq = Helpers.bind(prefix, defaultXml, vals.map(a => TheBindParam(a._1, a._2)) :_*)
  def bind(vals: (String, NodeSeq)*): NodeSeq = bind(defaultPrefix, vals :_*)
  
  def ask(who: CometActor, what: Any)(answerWith: Any => Any) {
    who.start
    theSession.addCometActor(who)
    who.link(self)
    who ! PerformSetupComet
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this)
  }
  
  def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, S.request.open_!))
    whosAsking = Empty
    reRender(false)
  }
  
  implicit def xmlToXmlOrJsCmd(in: NodeSeq): RenderOut = new RenderOut(Full(in), fixedRender, Empty, Empty, false)
  // implicit def xmlNsToXmlOrJsCmd(in: Seq[Node]): RenderOut = new RenderOut(in)
  implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = new RenderOut(Empty, Empty, Full(in), Empty, false)
  implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = (in._1, Text(in._2 match {case null => "null" case s => s.toString}))
  implicit def nodeSeqToFull(in: NodeSeq): Can[NodeSeq] = Full(in)
  implicit def elemToFull(in: Elem): Can[NodeSeq] = Full(in)
}

abstract class Delta(val when: Long) {
  def js: JsCmd
}

case class JsDelta(override val when: Long, js: JsCmd) extends Delta(when)

sealed abstract class CometMessage

class XmlOrJsCmd(val id: String,val xml: Can[NodeSeq],val fixedXhtml: Can[NodeSeq], val javaScript: Can[JsCmd], val destroy: Can[JsCmd],
    spanFunc: (Long, NodeSeq) => NodeSeq, ignoreHtmlOnJs: Boolean) {
  def this(id: String, ro: RenderOut, spanFunc: (Long, NodeSeq) => NodeSeq) =  this(id, ro.xhtml,ro.fixedXhtml, ro.script, ro.destroyScript, spanFunc, ro.ignoreHtmlOnJs)
  def toJavaScript(session: LiftSession, displayAll: Boolean): JsCmd = {
    val ret: JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_"+id+"();"), false) ++
    ((if (ignoreHtmlOnJs) Empty else xml, javaScript, displayAll) match { 
    case (Full(xml), Full(js), false) => JsCmds.SetHtml(id, xml) ++ JsCmds.JsTry(js, false)
    // case (Full(xml), Full(js), false) => JsCmds.SetHtml(id, session.processSurroundAndInclude("Comet id: "+id, xml)) ++ JsCmds.JsTry(js, false)
    
    // case (Full(xml), _, false) => JsCmds.SetHtml(id, session.processSurroundAndInclude(xml))
    case (Full(xml), _, false) => JsCmds.SetHtml(id, xml)
    
//    case (Full(xml), Full(js), true) => JsCmds.SetHtml(id+"_outer", session.processSurroundAndInclude(<span id={id}>{xml}</span> ++
//      fixedXhtml.openOr(Text("")))) ++ JsCmds.JsTry(js, false)
      
//    case (Full(xml), _, true) => JsCmds.SetHtml(id+"_outer", session.processSurroundAndInclude(<span id={id}>{xml}</span> ++
//      fixedXhtml.openOr(Text(""))))

    case (Full(xml), Full(js), true) => JsCmds.SetHtml(id+"_outer", (spanFunc(0, xml) ++
      fixedXhtml.openOr(Text("")))) ++ JsCmds.JsTry(js, false)
      
    case (Full(xml), _, true) => JsCmds.SetHtml(id+"_outer", (spanFunc(0, xml) ++
      fixedXhtml.openOr(Text(""))))
    
    case (_, Full(js), _) => js
    
    case _ => JsCmds.Noop
  }) ++ JsCmds.JsTry(JsCmds.Run("destroy_"+id+" = function() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"};"), false)
      ret
  }

  def inSpan: NodeSeq = xml.openOr(Text(""))++javaScript.map(s => Script(s)).openOr(Text("")) 
  def outSpan: NodeSeq = Script(Run("var destroy_"+id+" = function() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"}")) ++
    javaScript.map(s => Script(s)).openOr(Text("")) ++ fixedXhtml.openOr(Text(""))
  //def asXhtml: NodeSeq = xml.openOr(Text(""))
}

case object AskRender extends CometMessage
case class AnswerRender(response: XmlOrJsCmd, who: CometActor, when: Long, displayAll: Boolean) extends CometMessage
case object PerformSetupComet extends CometMessage
case class AskQuestion(what: Any, who: CometActor) extends CometMessage
case class AnswerQuestion(what: Any, request: RequestState) extends CometMessage
case class Listen(when: Long) extends CometMessage
case object Unlisten extends CometMessage
// case class ActionMessage(what: S.AFuncHolder, value: List[String], request: RequestState) extends CometMessage
case class ActionMessageSet(msg: List[() => Any], request: RequestState) extends CometMessage
case class ReRender(doAll: Boolean) extends CometMessage

/**
 * @param xhtml is the "normal" render body
 * @param fixedXhtml is the "fixed" part of the body.  This is ignored unless reRender(true)
 * @param script is the script to be executed on render.  This is where you want to put your script
 * @param destroyScript is executed when the comet widget is redrawn ( e.g., if you register drag or mouse-over or some events, you unregister them here so the page doesn't leak resources.)
 * @param ignoreHtmlOnJs -- if the reason for sending the render is a Comet update, ignore the xhtml part and just run the JS commands.  This is useful in IE when you need to redraw the stuff inside <table><tr><td>... just doing innerHtml on <tr> is broken in IE 
 */
@serializable
case class RenderOut(xhtml: Can[NodeSeq], fixedXhtml: Can[NodeSeq], script: Can[JsCmd], destroyScript: Can[JsCmd], ignoreHtmlOnJs: Boolean) {
  def this(xhtml: NodeSeq) = this(Full(xhtml), Empty, Empty, Empty, false)
  def this(xhtml: NodeSeq, js: JsCmd) = this(Full(xhtml), Empty, Full(js), Empty, false)
  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this(Full(xhtml), Empty, Full(js), Full(destroy), false) 
}
