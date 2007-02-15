package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import scala.xml.{NodeSeq, Elem, Node, Comment}
import scala.collection.immutable.{TreeMap, ListMap}
import scala.collection.mutable.{HashMap}
import net.liftweb.util.Helpers._

class Page extends Actor {
  private var pageXml : NodeSeq = _
  private var theSession: Session = _
  private var globalState: TreeMap[String, Any] = _
  private var localState = new TreeMap[String, Any]
  private var request: RequestState = _
  
  private var messageCallback: Map[String, ActionMessage] = TreeMap.Empty[String, ActionMessage]
  
  def act = loop
  
  def loop {
    react(dispatcher)
  }
  
  def dispatcher: PartialFunction[Any, Unit] = {
    case "shutdown" => self.exit("shutdown")
    case SetupPage(page, session) => {
      pageXml = page
      this.theSession = session
      loop
    }
    
    case RenderPage(state, sessionState, sender) =>
      this.globalState = sessionState
    this.request = state
    try {
    val resp : Response = try {
      processParameters(state)
      
      Response(processControllers(pageXml), TreeMap.empty, 200)
      
    } catch {
      case rd : RedirectException => {   
	Response(<html><body>{state.uri} Not Found</body></html>,
                 ListMap("Location" -> rd.to),
                 302)
      }
    }
    
    sender ! RenderedPage(state, resp, sender)
    } finally {
    this.globalState = null
    this.request = null
    }
    loop
    
    
    case _ => loop
  }
  
  def apply[T](name: String): Option[T] = {
    (localState.get(name) match {
      case None => globalState.get(name)
      case s @ Some(_) => s
    }) match {
      case None => None
      case Some(s) if (s.isInstanceOf[T]) => Some(s.asInstanceOf[T])
      case _ => None
    }
  }
  
  def update(key: String, value: Any) = {
    localState= (localState(key) = value)
  }
  
  def globalUpdate(key: String, value: Any) = {
    globalState = (globalState(key) = value)
      theSession ! SetGlobal(key, value)
  }
  
  def globalRemove(key: String) = {
    globalState = (globalState - key)
      theSession ! UnsetGlobal(key)
  }

  
  private def processControllers(xml : NodeSeq) : NodeSeq = {
    xml.flatMap {
      v =>
        v match {
          case Elem("mondo", "controller", attr @ _, _, kids @ _*) => {findController(attr.get("type"), attr.get("name"), attr.get("factory"), processControllers(kids))}
          case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processControllers(v.child) : _*)}
          case _ => {v}
        }
    }
  }
  
  private var controllers = TreeMap.Empty[String, ControllerActor]
  
  private def locateController(name : String, contType: Option[Seq[Node]], factory: Option[Seq[Node]], kids: NodeSeq) : Option[ControllerActor] = {
    controllers.get(name) match {
      case s @ Some(c : ControllerActor) => s
      case None => {
        searchFactoryForController(contType, factory) match {
          case None => None
          case Some(ret) => 
          ret.start
          ret ! SetupController(this, kids)
          controllers = controllers(name) = ret
          Console.println("Updated our list of controllers")
          Some(ret)
	}
      }
    }
  }
  
  private def findFactory(factory: Option[Seq[Node]]) : Option[ControllerFactory] = {
    if (factory == None) None
    else {
      findClass(factory.get.text, "mondo.app.factory" :: Nil, {c : Class => classOf[ControllerFactory].isAssignableFrom(c)}) match {
        case None => None
        case Some(cls) => {
          try {
            Some(cls.newInstance.asInstanceOf[ControllerFactory])
          } catch {
            case _ => None
          }
        }
      }
    }
  }
  
  private def findControllerByType(contType: Option[Seq[Node]]): Option[ControllerActor] = {
    if (contType == None) None
    else {
      Console.println("Looking for "+contType)
      findClass(contType.get.text, "mondo.app.controller" :: Nil, {c : Class => classOf[ControllerActor].isAssignableFrom(c)}) match {
        case None => None
        case Some(cls) => {
          Console.println("Found "+cls)
          try {
            val ret = Some(cls.newInstance.asInstanceOf[ControllerActor])
            Console.println("Ret is "+ret)
            ret
          } catch {
            case _ => None
          }
        }
      }
    }
  }
  
  private def searchFactoryForController(contType: Option[Seq[Node]], factory: Option[Seq[Node]]) : Option[ControllerActor] = {
    if (contType == None && factory == None) None
    else {
      findFactory(factory) match {
        case Some(f) => {
          f.construct(contType)
        }
        case None => {
          findControllerByType(contType) match {
            case None => None
            case s @ Some(c : ControllerActor) => s
          }
        }
      }
    }
  }
  
  private def findController(controllerType : Option[Seq[Node]], controllerName : Option[Seq[Node]], controllerFactory : Option[Seq[Node]], kids : NodeSeq) : NodeSeq = {
    if (controllerType == None && controllerName == None) Comment("FIX"+"ME No controller name specified") concat kids
    else {
      val actualName = controllerName match {
	case Some(s) => s.text
	case None => controllerType.get.text
      }
      
      try {
        locateController(actualName, controllerType, controllerFactory, kids) match {
          case None => Comment("FIX"+"ME Can't find controller "+actualName) concat kids
          case Some(controller) => <span id={controller.uniqueId}>{
            (controller !? (600L, Render(globalState ++ localState.keys.map{k => {k, localState(k)}}))) match {
              case Some(view: Rendered) => updateCallbacks(view.messages) ; view.xml 
              case s2 @ _ => Comment("FIX"+"ME controller "+actualName+" timeout") concat kids
            }
          }</span>
        }
      } catch {
	case e @ _ => {
	  
	  <pre>{e.getMessage+"\n"}
	  {e.getStackTrace.toList.map{st => st.toString+"\n"}}
	  </pre>
	}
      }
    }
  }
  
  private def updateCallbacks(in: Map[String, ActionMessage]) {
    messageCallback = messageCallback ++ in
  }
  
  private def processParameters(r: RequestState) {
    r.paramNames.filter{n => messageCallback.contains(n)}.foreach{
      n => 
    	val v = messageCallback(n)
      v.target !? (100l, ActionMessage(v.name, r.params(n), self, Some(this)))
    }
    messageCallback = TreeMap.Empty[String, ActionMessage]
  }
}

abstract class PageMessage

case class SetupPage(page: NodeSeq, session: Session) extends PageMessage
case class Perform(localFunc: String, params: List[String]) extends PageMessage
//case class Render extends PageMessage
//case class Rendering(info: String, tpe: String) extends PageMessage
// case class ComponentUpdated(view: NodeSeq, component: Component) extends PageMessage
