package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import scala.xml.{NodeSeq, Text}
import scala.collection.immutable.TreeMap

trait ControllerActor extends Actor {
  private object Never
  
  val uniqueId = randomString(20)
  
  private var globalState: Map[String, Any] = _
  
  private var owner_i: Page = _
  private var defaultXml_i: NodeSeq = _
  private var localFunctionMap: Map[String, (List[String]) => boolean] = _
  
  private var askingWho: ControllerActor = _
  private var whosAsking: Actor = _
  private var answerWith: (Any) => boolean = _
  
  def act = loop
  
  def loop : Unit = {
    react(composeFunction)
  }  

  def highPriority : PartialFunction[Any, Unit] = {
    case Never => loop
  }
  
  def lowPriority : PartialFunction[Any, Unit] = {
    case Never => loop
  }
  
  def mediumPriority : PartialFunction[Any, Unit] = {
    case SetupController(owner, defaultXml) => {
      owner_i = owner
      defaultXml_i = defaultXml
      Console.println("Hey... I'm in the setup")
      localSetup
      loop
    }
    
    case r @ Render(state) => {
      if (askingWho != null) {
      	askingWho forward r
      } else {
      this.globalState = state
      reply(buildRendered(render))
      this.globalState = null
      }
      loop
    }
    
    case ActionMessage(name, value, _, replyTo) => {
      replyTo match {
        case Some(p:Page) => p ! StartedUpdate(uniqueId)
      }
      localFunctionMap.get(name) match {
        case Some(f) => {

          if (f(value)) reRender
        }
        case _ => None
      }
      replyTo match {
      case Some(p:Page) => p ! FinishedUpdate(uniqueId)
    }

      loop
    }
    
    case Exit(_, reason) => {
      self.exit(reason)
      loop
    }
    
    case Ask(what, who) => {
      startQuestion(what)
      whosAsking = who
      loop
    }
    
    case Answer(what) => {
      askingWho.unlink(self)
      askingWho ! Exit(self, "bye")
      askingWho = null
      if (answerWith(what)) reRender
      answerWith = null
      reply("Done")
      loop
    }
  }
  
  def render: XmlAndMap
  
  def compute: Map[String, Any] = Map.empty[String, Any]
                   
  def reRender = {
    if (owner != null) owner ! buildRendered(render)
  }
  
  def startQuestion(what: Any) {}
  
  def localSetup {}
  
  def composeFunction = composeFunction_i
  
  val composeFunction_i = {
    val cleanUp: PartialFunction[Any, Unit] = {
      case _ => loop
    }
    highPriority orElse mediumPriority orElse lowPriority orElse cleanUp
  }
  
  def owner = owner_i
  def defaultXml = defaultXml_i
  
  private def buildRendered(in: XmlAndMap): Rendered = {
    localFunctionMap = in.map
    val newMap = TreeMap.Empty[String, ActionMessage] ++ in.map.keys.map{key => {key, ActionMessage(key, Nil, self, None)}}
    Rendered(in.xml, newMap)
  }
  
  def ask(who: ControllerActor, what: Any)(answerWith: (Any) => boolean) {
    who.start
    who.link(self)
    who ! SetupController(owner, Text(""))
    askingWho = who
    this.answerWith = answerWith
    who ! Ask(what, self)
  }
  
  def answer(answer: Any) {
    whosAsking !? Answer(answer)
    whosAsking = null
    reRender
  }

}

sealed abstract class ControllerMessage

case class Render(state: Map[String, Any]) extends ControllerMessage
case class ActionMessage(name: String, value: List[String], target: Actor, sender: Option[Page]) extends ControllerMessage
case class Rendered(xml : NodeSeq, messages: Map[String, ActionMessage] ) extends ControllerMessage
case class SetupController(owner: Page, defaultXml: NodeSeq) extends ControllerMessage
case class XmlAndMap(xml: NodeSeq, map: Map[String, Function1[List[String], boolean]]) extends ControllerMessage
case class Ask(what: Any, who: Actor) extends ControllerMessage
case class Answer(what: Any) extends ControllerMessage
case class StartedUpdate(id: String) extends ControllerMessage
case class FinishedUpdate(id: String) extends ControllerMessage


