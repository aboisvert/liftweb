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
import scala.xml.{NodeSeq, Text, Elem, Unparsed, Node}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{HashSet, ListBuffer}
import S._
import net.liftweb.http.js._

// import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent}

@serializable 
abstract class CometActor(val theSession: LiftSession, val name: Can[String], val defaultXml: NodeSeq, val attributes: Map[String, String]) extends Actor {
  
  @serializable
  private object Never
  val uniqueId = "LC"+randomString(20)
  private var lastRenderTime = millis
  private var lastRendering: RenderOut = RenderOut(Full(defaultXml), Empty, Empty)
  private val listeners = new ListBuffer[Actor]()
  // private var globalState: Map[String, Any] = _

  /// private var localFunctionMap: Map[String, AFuncHolder] = Map.empty
  private var askingWho: Can[CometActor] = Empty
  private var whosAsking: Can[CometActor] = Empty
  private var answerWith: Can[Any => Any] = Empty
  private var sessionVars: Map[String, String] = Map.empty
  
  def defaultPrefix: String
  
  /**
    * Set to 'true' if we should run "render" on every page load
    */
  protected def devMode = false
  
  def act = {
    this.trapExit = true
    loop {
    react(composeFunction)
    }
  }

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
        if (when >= lastRenderTime) listeners += sender.receiver
        else sender.receiver ! AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering), whosAsking openOr this, lastRenderTime)
    }
    
    case PerformSetupComet(sessionVars) =>
      this.sessionVars = sessionVars
      localSetup
      reRender
    
    case AskRender =>
      askingWho match {
        case Full(who) => who forward AskRender
        case _ => if (devMode) reRender; reply(AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering), whosAsking openOr this, lastRenderTime))
        }
    
    case ActionMessageSet(msgs, sv) =>
    this.sessionVars = sv
    S.init(request, theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
      val ret = msgs.map(msg => msg.what(msg.value))
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      reply(ret)
    }    
    
    case ActionMessage(what, value, _, sv) => 
    this.sessionVars = sv
      S.init(request, theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
      val ret = what(value)
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
        reply(ret)
      }

    case AskQuestion(what, who) =>
      startQuestion(what)
      whosAsking = Full(who)
    
    case AnswerQuestion(what, request) =>
      S.init(request, theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
        askingWho.foreach {
          askingWho =>
        reply("A null message to release the actor from its send and await reply... do not delete this message")
	askingWho.unlink(self)
        askingWho ! ShutDown
	this.askingWho = Empty
        val aw = answerWith
        answerWith = Empty
	aw.foreach(_(what))
        reRender
        }
      }
      
    case ReRender => reRender

    case ShutDown =>
    theSession.removeCometActor(this)
    localShutdown()
    self.exit("Politely Asked to Exit")
  }
  
  def render: RenderOut
  
  def compute: Map[String, Any] = Map.empty[String, Any]
  
  final def reRender(): AnswerRender = {
    lastRenderTime = millis
    S.initIfUninitted(theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
      lastRendering = render
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      val rendered: AnswerRender = AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering), this, lastRenderTime)// buildRendered(lastRendering, lastRenderTime)
      listeners.toList.foreach(_ ! rendered)
      listeners.clear
      rendered
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
    who ! PerformSetupComet(sessionVars)
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this)
  }
  
  def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, S.request))
    whosAsking = Empty
    reRender
  }
  
  implicit def xmlToXmlOrJsCmd(in: NodeSeq): RenderOut = new RenderOut(in)
  // implicit def xmlNsToXmlOrJsCmd(in: Seq[Node]): RenderOut = new RenderOut(in)
  implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = new RenderOut(Empty, Full(in), Empty)
}

sealed abstract class CometMessage

class XmlOrJsCmd(val id: String,val xml: Can[NodeSeq],val javaScript: Can[JsCmd], val destroy: Can[JsCmd]) {
  def this(id: String, ro: RenderOut) = this(id, ro.xhtml, ro.script, ro.destroyScript)
  def toJavaScript(session: LiftSession): JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_"+id+"();")) + 
    ((xml, javaScript) match {
    case (Full(xml), Full(js)) => JsCmds.Set(id, session.processSurroundAndInclude(xml)) + JsCmds.JsTry(js)
    case (Full(xml), _) => JsCmds.Set(id, session.processSurroundAndInclude(xml))
    case (_, Full(js)) => js
    case _ => JsCmds.Noop
  }) + JsCmds.JsTry(JsCmds.Run("destroy_"+id+" = function() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"};"))
  
  def asXhtml: NodeSeq = ((xml, javaScript) match {
  case (Full(xml), Full(js)) => xml ++ script(js.toJsCmd)
  case (Full(xml), _) => xml
  case (_, Full(js)) => script(js.toJsCmd)
  case _ => Text("")
  }) ++ script("function destroy_"+id+"() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"}")
    
  //def asXhtml: NodeSeq = xml.openOr(Text(""))
}

case object AskRender extends CometMessage
case class AnswerRender(response: XmlOrJsCmd, who: CometActor, when: Long ) extends CometMessage
case class PerformSetupComet(sessionVars: Map[String, String]) extends CometMessage
case class AskQuestion(what: Any, who: CometActor) extends CometMessage
case class AnswerQuestion(what: Any, request: RequestState) extends CometMessage
case class Listen(when: Long) extends CometMessage
case object Unlisten extends CometMessage
case class ActionMessage(what: AFuncHolder, value: List[String], target: Actor, sessionVars: Map[String, String]) extends CometMessage
case class ActionMessageSet(msg: List[ActionMessage], sessionVars: Map[String, String]) extends CometMessage
case object ReRender extends CometMessage
case class RenderOut(xhtml: Can[NodeSeq], script: Can[JsCmd], destroyScript: Can[JsCmd]) {
  def this(xhtml: NodeSeq) = this(Full(xhtml), Empty, Empty)
  def this(xhtml: NodeSeq, js: JsCmd) = this(Full(xhtml), Full(js), Empty)
  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this(Full(xhtml), Full(js), Full(destroy)) 
}