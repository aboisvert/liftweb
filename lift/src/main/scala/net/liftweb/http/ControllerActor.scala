package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import scala.xml.{NodeSeq, Text, Elem}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashSet
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent}


trait ControllerActor extends Actor /*with HttpSessionActivationListener*/ {
  private object Never
  
  val uniqueId = randomString(20)
  
  private var globalState: Map[String, Any] = _
  
  private var owner_i = new HashSet[Page]
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
    case PerformSetupController(owner, defaultXml) => {
      owner.foreach{o => owner_i += o}
      defaultXml_i = defaultXml
      localSetup
      loop
    }
    
    case r @ AskRender(state) => {
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
    
    case AskQuestion(what, who) => {
      startQuestion(what)
      whosAsking = who
      loop
    }
    
    case AnswerQuestion(what) => {
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
    val rendered = buildRendered(render)
    owner.foreach{o => o ! rendered}
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
  
  def bind(vals: Map[String, NodeSeq]): NodeSeq = {
    def _bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
        node match {
          case Elem("lift", "bind", attr, foo, kids) => {
            attr.get("name") match {
              case None => _bind(vals, kids)
              case Some(ns) => {
                vals.get(ns.text) match {
                  case None => _bind(vals, kids)
                  case Some(nodes) => nodes
                }
              }
            }
          }
          case Elem(ns, tag, attr, foo, kids) => Elem(ns, tag, attr,foo, _bind(vals, kids) : _*)
          case n => node
        }
      }
    }
    
    _bind(vals, defaultXml)
  }
  
  private def buildRendered(in: XmlAndMap): AnswerRender = {
    localFunctionMap = in.map
    val newMap = TreeMap.Empty[String, ActionMessage] ++ in.map.keys.map{key => {key, ActionMessage(key, Nil, self, None)}}
    AnswerRender(in.xml, newMap, this)
  }
  
  def ask(who: ControllerActor, what: Any)(answerWith: (Any) => boolean) {
    who.start
    who.link(self)
    who ! PerformSetupController(owner.toList, Text(""))
    askingWho = who
    this.answerWith = answerWith
    who ! AskQuestion(what, self)
  }
  
  def answer(answer: Any) {
    whosAsking !? AnswerQuestion(answer)
    whosAsking = null
    reRender
  }
  
  /*
  def sessionDidActivate(se: HttpSessionEvent) = {
    Console.println("Did activate")
  }
def sessionWillPassivate(se: HttpSessionEvent) = {
  val session = se.getSession
  val atNames = session.getAttributeNames
  while (atNames.hasMoreElements) {
    atNames.nextElement match {
      
      case s: String => Console.println("Removed "+s); session.removeAttribute(s)
      case o => Console.println("Didn't remove "+o)
    }
  }
  Console.println("Did passivate real good")
}
*/

}

sealed abstract class ControllerMessage

case class AskRender(state: Map[String, Any]) extends ControllerMessage
case class ActionMessage(name: String, value: List[String], target: Actor, sender: Option[Page]) extends ControllerMessage
case class AnswerRender(xml : NodeSeq, messages: Map[String, ActionMessage], by: ControllerActor ) extends ControllerMessage
case class PerformSetupController(owner: List[Page], defaultXml: NodeSeq) extends ControllerMessage
case class XmlAndMap(xml: NodeSeq, map: Map[String, Function1[List[String], boolean]]) extends ControllerMessage
case class AskQuestion(what: Any, who: Actor) extends ControllerMessage
case class AnswerQuestion(what: Any) extends ControllerMessage
case class StartedUpdate(id: String) extends ControllerMessage
case class FinishedUpdate(id: String) extends ControllerMessage


