package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors._
import scala.actors.Actor._
import scala.xml.{NodeSeq, Elem, Node, Comment, Unparsed, MetaData, UnprefixedAttribute, Text}
import scala.collection.immutable.{TreeMap, ListMap}
import scala.collection.mutable.{HashMap}
import net.liftweb.util.Helpers._

class Page extends Actor {
  private var pageXml : NodeSeq = _
  private var theSession: Session = _
  private var globalState: TreeMap[String, Any] = _
  private var localState = new HashMap[String, Any]
  private var request: RequestState = _
  private var updates = new HashMap[String, AnswerRender]
  private var messageCallback: Map[String, ActionMessage] = TreeMap.Empty[String, ActionMessage]
  
  def act = loop
  
  def loop {
    react(dispatcher)
  }
  
  def dispatcher: PartialFunction[Any, Unit] = {
    case "shutdown" => self.exit("shutdown")
    case PerformSetupPage(page, session) => {
      pageXml = page
      this.theSession = session
      loop
    }
    
    case AskRenderPage(state, sessionState, sender, controllerMgr, timeout) =>
      this.globalState = sessionState
    this.request = state
    try {
      processParameters(state)
      
      val resp : Response = if (state.ajax_?) {
        // wait for redraws
        val endAt = System.currentTimeMillis + (timeout - 1000L)
        
        while (updates.isEmpty && endAt > System.currentTimeMillis) {
          receiveWithin(endAt - System.currentTimeMillis) {
            case ar: AnswerRender => updateRendered(ar)
            case PerformSetupPage(page, session) => {
	      pageXml = page
	      this.theSession = session
            }
            
            case AskRenderPage(state, sessionState, sender, controllerMgr, timeout) => {
	      processParameters(state)
	      val resp: Response = if (state.ajax_? ) {
		Response(Unparsed(""),
			 Map("Content-Type" -> "text/javascript"), 200)
	      } else {
		try {
		  Response(state.fixHtml(processControllers(pageXml, controllerMgr, state)), TreeMap.empty, 200)
		} catch {
                  case rd : RedirectException => {   
                    Response(state.fixHtml(<html><body>{state.uri} Not Found</body></html>),
                             ListMap("Location" -> rd.to),
                             302)
                  }
                  case e  => state.showException(e)
		}
	      }
	      reply(resp)
            }
            case TIMEOUT => null
          }
        }
	
        val ret = updates.map{
          pl => 
            val uid = pl._1
          val ar = pl._2
          val html = updateCallbacks(ar, state).toString
          "try{$('"+uid+"').innerHTML = decodeURIComponent('"+urlEncode(html)+"'.replace(/\\+/g,'%20'))} catch (e) {}"
        }.mkString("", "\n", "")
        
        Response(Unparsed(ret),Map("Content-Type" -> "text/javascript"), 200)
      } else {
        try {

	  Response(state.fixHtml(processControllers(pageXml, controllerMgr, state)), TreeMap.empty, 200)
	  
	} catch {
	  case rd : RedirectException => {   
	    Response(state.fixHtml(<html><body>{state.uri} Not Found</body></html>),
                     ListMap("Location" -> rd.to),
                     302)
	  }
	  case e  => state.showException(e)
	  
	}
      }
      this.updates = new HashMap
      reply(resp)
    } finally {
      this.globalState = null
      this.request = null
    }
    loop
    
    case ar: AnswerRender => updateRendered(ar)
    loop
    
    case _ => loop
  }
  
  def updateRendered(ar: AnswerRender) {
    updates(ar.by.uniqueId) = ar
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
    localState(key) = value
  }
  
  def globalUpdate(key: String, value: Any) = {
    globalState = (globalState(key) = value)
      theSession ! SetGlobal(key, value)
  }
  
  def globalRemove(key: String) = {
    globalState = (globalState - key)
      theSession ! UnsetGlobal(key)
  }

  
  private def processControllers(xml : NodeSeq, ctlMgr: ControllerManager, request: RequestState) : NodeSeq = {
    xml.flatMap {
      v =>
        v match {
          case Elem("lift", "controller", attr @ _, _, kids @ _*) => {executeController(ctlMgr, attr.get("type"), attr.get("name"), attr.get("factory"), processControllers(kids, ctlMgr, request), request)}
          case Elem("lift", "ctl", attr @ _, _, kids @ _*) => {executeControllerette(ctlMgr, attr.get("type"), attr.get("name"), attr.get("factory"), processControllers(kids, ctlMgr, request), request)}
          case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processControllers(v.child, ctlMgr, request) : _*)}
          case _ => {v}
        }
    }
  }
  
  private def executeController(ctlMgr: ControllerManager, 
				theType: Option[Seq[Node]], 
				name: Option[Seq[Node]], 
				factory: Option[Seq[Node]], kids: NodeSeq,
				request: RequestState): NodeSeq
  = 
    {
      val (myType, myName, myFactory) = (theType.map{s => s.text},name.map{s => s.text}, factory.map{s => s.text})
	(ctlMgr !? AskFindController(myType, myName, myFactory) match {
	  case AnswerFoundController(controller) => controller
	  case _ => None
	}).map{
	  controller => 
	    // set up the controller
	    controller ! PerformSetupController(List(this), kids)
	  <span id={controller.uniqueId}>{
	    (controller !? (600L, AskRender(globalState ++ localState.keys.map{k => (k, localState(k))}, request))) match {
	      case Some(view: AnswerRender) => updateCallbacks(view, request) 
	      case _ => Comment("FIX"+"ME controller type "+myType+" name "+myName+" timeout") concat kids
	    }
	  }</span>
	} getOrElse {
	  Comment("FIX"+"ME -- Controller type: "+myType+" name: "+myName+" factory "+myFactory+" Not Found ") concat kids
	}
    }

  private def executeControllerette(ctlMgr: ControllerManager, 
        theType: Option[Seq[Node]], 
        method: Option[Seq[Node]], 
        factory: Option[Seq[Node]], kids: NodeSeq,
        request: RequestState): NodeSeq
  = 
    {
      // val (myType, myMethod, myFactory) = (theType.map{s => s.text},method.map{s => s.text}, factory.map{s => s.text})
      val func: Option[(NodeSeq) => NodeSeq] = for (val typ <- theType.map(_.text);
           val meth <- method.map(_.text);
           val resp <- (ctlMgr !? AskFindControllerette(typ, meth, factory.map(_.text))) match {
	       case AnswerFoundControllerette(controller) => controller
	       case _ => None
	     }) yield resp

       func.map(_.apply(kids)) getOrElse (Comment("FIX"+"ME -- Controller type: "+theType+" method: "+method+" factory "+factory+" Not Found ") concat kids)
    }
  
  private def updateCallbacks(in: AnswerRender, request: RequestState): NodeSeq = {
    messageCallback = messageCallback ++ in.messages
    val ret = processForForms(in.xml, request)
    ret
  }
  
  private def processForForms(xml : NodeSeq,  request: RequestState) : NodeSeq = {
    xml.flatMap {
      v =>
        v match {
          case Elem("lift", "form", attr @ _ , scope @ _, kids @ _*) => {Elem(null, "form", addAjaxOnSubmit(request, attr), scope, processForForms(kids, request): _*)}
          case Elem(prefix @ _,label @ _,attr @ _,scope @ _,kids @ _*) => {Elem(prefix, label, attr, scope, processForForms(kids, request) : _*)}
          case _ => {v}
        }
    }
  }
  
  private def addAjaxOnSubmit(request: RequestState, attr: MetaData): MetaData = {
    val ajax = "new Ajax.Request('"+request.contextPath+request.uri+"', {asynchronous:true, parameters:Form.serialize(this), method: 'put', requestHeaders:{ Accept:'text/javascript' }}); return false;"
    new UnprefixedAttribute("onsubmit", ajax, attr)
  }

  
  private def processParameters(r: RequestState) {
    r.paramNames.filter{n => messageCallback.contains(n)}.foreach{
      n => 
    	val v = messageCallback(n)
      v.target !? (100l, ActionMessage(v.name, r.params(n), self, Some(this), r))
    }
    // messageCallback = TreeMap.Empty[String, ActionMessage]
  }
}

abstract class PageMessage

case class PerformSetupPage(page: NodeSeq, session: Session) extends PageMessage
// case class Perform(localFunc: String, params: List[String]) extends PageMessage
//case class Render extends PageMessage
//case class Rendering(info: String, tpe: String) extends PageMessage
// case class ComponentUpdated(view: NodeSeq, component: Component) extends PageMessage
