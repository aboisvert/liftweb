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
import scala.xml.{NodeSeq, Text, Elem}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashSet
import S._

import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent}

trait ControllerActor extends Actor /*with HttpSessionActivationListener*/ {
  private object Never
  private var _name = ""
  private var attributes: Map[String, String] = Map.empty
  val uniqueId = "Id"+randomString(20)
  
  // private var globalState: Map[String, Any] = _
  
  private var owner_i = new HashSet[Page]
  private var defaultXml_i: NodeSeq = _
  private var localFunctionMap: Map[String, AFuncHolder] = Map.empty
  private var theSession: Session = _
  private var askingWho: Can[ControllerActor] = Empty
  private var whosAsking: Can[Actor] = Empty
  private var answerWith: Can[(Any) => boolean] = Empty
  private var sessionVars: Map[String, String] = Map.empty
  
  def name = _name
  def act = {
    this.trapExit = true
    loop {
    react(composeFunction)
    }
  }
  
  /*def loop : Unit = {
    react(composeFunction)
  } */ 

  def highPriority : PartialFunction[Any, Unit] = {
    case Never =>
  }
  
  def lowPriority : PartialFunction[Any, Unit] = {
    case s => Log.debug("Controller "+this+" got unexpected message "+s)
  }
  
  def mediumPriority : PartialFunction[Any, Unit] = {
    case SetName(theName) =>
    this._name = theName
    
    case PerformSetupController(owner, defaultXml, attributes, session, sessionVars) =>
      this.theSession = session
      this.sessionVars = sessionVars
      this.attributes = attributes
      owner.foreach{o => owner_i += o}
      defaultXml_i = defaultXml
      localSetup
    
    case r @ AskRender(request) =>
      if (!askingWho.isEmpty) {
      	askingWho.open forward r
      } else {
          S.init(request,theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
	    reply(buildRendered(render))
          }
      }
    
    case ActionMessage(name, value, _, replyTo, request, sv) =>
    this.sessionVars = sv
      S.init(request,theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
	Can(localFunctionMap.get(name)) match {
          case Full(f) => {

            if (f(value)) reRender
          }
          case _ => Empty
	}
        reply(sessionVars)
      }

    case AskQuestion(what, who) =>
      startQuestion(what)
      whosAsking = Full(who)
    
    case AnswerQuestion(what, request) =>
      S.init(request,theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
        askingWho.foreach {
          askingWho =>
        reply("A null message to release the actor from its send and await reply... do not delete this message")
	askingWho.unlink(self)
        askingWho ! DoExit
	this.askingWho = Empty
	if (answerWith.map(f => f(what)) openOr false) reRender
	answerWith = Empty
        }
      }

    case DoExit => self.exit("Politely Asked to Exit")
  }
  
  def render: NodeSeq
  
  def compute: Map[String, Any] = Map.empty[String, Any]
  
  def reRender = {
    S.initIfUninitted(theSession, new VarStateHolder(theSession, sessionVars, Full(sessionVars_= _), false)) {
      val rendered: AnswerRender = buildRendered(render)
      owner.foreach(_ ! rendered)
    }
  }
  
  def startQuestion(what: Any) {}
  
  def localSetup {}
  
  def composeFunction = composeFunction_i
  
  def composeFunction_i = highPriority orElse mediumPriority orElse lowPriority
  
  def owner = owner_i
  def defaultXml = defaultXml_i
  
  def bind(vals: (String, NodeSeq)*): NodeSeq = {
    Helpers.bind(Map.empty ++ vals, defaultXml)
  }
  
  private def buildRendered(in: NodeSeq): AnswerRender = {
    localFunctionMap = localFunctionMap ++ S.getFunctionMap
    val newMap = TreeMap.empty[String, ActionMessage] ++ localFunctionMap.keys.map{key => (key, ActionMessage(key, Nil, self, Empty, null, Map.empty))}
    AnswerRender(in, newMap, this)
  }
  
  def ask(who: ControllerActor, what: Any)(answerWith: (Any) => boolean) {
    who.start
    who.link(self)
    who ! PerformSetupController(owner.toList, Text(""), attributes, theSession, sessionVars)
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, self)
  }
  
  def answer(answer: Any, request: RequestState) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, request))
    whosAsking = Empty
    reRender
  }
}

sealed abstract class ControllerMessage

case class AskRender(request: RequestState) extends ControllerMessage
case class ActionMessage(name: String, value: List[String], target: Actor, sender: Can[Page], request: RequestState, sessionVars: Map[String, String]) extends ControllerMessage
case class AnswerRender(xml : NodeSeq, messages: Map[String, ActionMessage], by: ControllerActor ) extends ControllerMessage
case class PerformSetupController(owner: List[Page], defaultXml: NodeSeq, attributes: Map[String, String], session: Session,
    sessionVars: Map[String, String]) extends ControllerMessage
case class XmlAndMap(xml: NodeSeq, map: Map[String, Function1[List[String], boolean]]) extends ControllerMessage
case class AskQuestion(what: Any, who: Actor) extends ControllerMessage
case class AnswerQuestion(what: Any, request: RequestState) extends ControllerMessage
case class StartedUpdate(id: String) extends ControllerMessage
case class FinishedUpdate(id: String) extends ControllerMessage
case class SetName(name: String) extends ControllerMessage
case object DoExit extends ControllerMessage

