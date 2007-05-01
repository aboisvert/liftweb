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
import net.liftweb.util.ActorPing

class Page extends Actor {
  private var updates = new HashMap[String, AnswerRender]
  private var messageCallback: Map[String, ActionMessage] = TreeMap.empty[String, ActionMessage]
  private var pendingAjax: List[AjaxRerender] = Nil
  
  def act = {
    this.trapExit = true
    loop(react(dispatcher))
  }
  

  def dispatcher: PartialFunction[Any, Unit] = {
    case "shutdown" => self.exit("shutdown")
    
    case AskRenderPage(state, pageXml, sender, controllerMgr, timeout) =>
      performRender(state, pageXml, sender, controllerMgr, timeout)
    
    case ar: AnswerRender => updateRendered(ar)
    
    case ajr : AjaxRerender =>
    if (pendingAjax.contains(ajr)) {
      ajr.sendTo ! XhtmlResponse(Unparsed(""), Map("Content-Type" -> "text/javascript"), 200)
      pendingAjax = pendingAjax.remove(_ eq ajr)
    }
    
    case unknown => Console.println("Page got "+unknown)
  }
  
  def updateRendered(ar: AnswerRender) {
    updates(ar.by.uniqueId) = ar
    if (!pendingAjax.isEmpty) {
      pendingAjax.foreach(ar => ar.sendTo ! buildResponseFromUpdates(ar.state))
      pendingAjax = Nil
      updates.clear
    }
  }
  
  private def buildResponseFromUpdates(state: RequestState): XhtmlResponse = {
    val ret = updates.map{
      pl => 
        val uid = pl._1
      val ar = pl._2
      val html = updateCallbacks(ar, state).toString
      "try{$('"+uid+"').innerHTML = decodeURIComponent('"+urlEncode(html)+"'.replace(/\\+/g,'%20'))} catch (e) {}"
    }.mkString("", "\n", "")
    
    XhtmlResponse(Unparsed(ret),Map("Content-Type" -> "text/javascript"), 200)
  }
  
  private def performRender(state: RequestState, pageXml: NodeSeq,
      sender: Actor, controllerMgr: ControllerManager, timeout: long) {
      processParameters(state)
      
      if (state.ajax_? && updates.isEmpty) {
        val ajaxer = AjaxRerender(System.currentTimeMillis + (timeout - 1000L), sender, state)
        ActorPing.schedule(self, ajaxer, timeout - 1000L)
        pendingAjax = ajaxer :: pendingAjax
      } else {
        val resp : XhtmlResponse = if (state.ajax_?) {
          val ret = buildResponseFromUpdates(state)
          updates.clear
          ret
        } else {
          try {
            XhtmlResponse(state.fixHtml(processControllers(pageXml, controllerMgr, state)), TreeMap.empty, 200)
          } catch {
            case rd : RedirectException => {   
              XhtmlResponse(state.fixHtml(<html><body>{state.uri} Not Found</body></html>),
                       ListMap("Location" -> rd.to),
                       302)
            }
            case e  => state.showException(e)
            
          }
        }
        sender ! resp
      }
  }
  
  private def processControllers(xml : NodeSeq, ctlMgr: ControllerManager, request: RequestState) : NodeSeq = {
    xml.flatMap {
      v =>
        v match {
          case Elem("lift", "controller", attr @ _, _, kids @ _*) => {executeController(ctlMgr, attr.get("type"), attr.get("name"), attr.get("factory"), processControllers(kids, ctlMgr, request), request)}
          case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processControllers(v.child, ctlMgr, request) : _*)}
          case _ => {v}
        }
    }
  }
  
  private def executeController(ctlMgr: ControllerManager, 
				theType: Option[Seq[Node]], 
				name: Option[Seq[Node]], 
				factory: Option[Seq[Node]], kids: NodeSeq,
				request: RequestState): NodeSeq = 
    {
    try {
      val (myType, myName, myFactory) = (theType.map(_.text),name.map(_.text), factory.map(_.text))
      val ret = (ctlMgr !? (1500l, AskFindController(myType, myName, myFactory)) match {
	  case Some(AnswerFoundController(controller)) => controller
	  case _ => None
	}).map{
	  controller => 
	    // set up the controller
	    controller ! PerformSetupController(List(this), kids)
	  <span id={controller.uniqueId}>{
	    (controller !? (600L, AskRender(request))) match {
	      case Some(view: AnswerRender) => updateCallbacks(view, request) 
	      case _ => Comment("FIX"+"ME controller type "+myType+" name "+myName+" timeout") ++ kids
	    }
	  }</span>
	} getOrElse {
	  Comment("FIX"+"ME - Controller type: "+myType+" name: "+myName+" factory "+myFactory+" Not Found ") ++ kids
	}
        
      ret
    } catch {
      case e => e.printStackTrace; kids
    }
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
        case Elem("lift", "a", attr @ _ , scope @ _, kids @ _*) => {Elem(null, "a", addAjaxHREF(request, attr), scope, processForForms(kids, request): _*)}
          case Elem(prefix @ _,label @ _,attr @ _,scope @ _,kids @ _*) => {Elem(prefix, label, attr, scope, processForForms(kids, request) : _*)}
          case _ => {v}
        }
    }
  }
  
  private def addAjaxOnSubmit(request: RequestState, attr: MetaData): MetaData = {
    val ajax = "new Ajax.Request('"+request.contextPath+request.uri+"', {asynchronous:true, parameters:Form.serialize(this), method: 'put', requestHeaders:{ Accept:'text/javascript' }}); return false;"
    new UnprefixedAttribute("onsubmit", ajax, attr)
  }

  private def addAjaxHREF(request: RequestState, attr: MetaData): MetaData = {
    val ajax = "javascript: new Ajax.Request('"+request.contextPath+request.uri+"', {asynchronous:true, parameters:'"+attr("key")+"=true', requestHeaders:{ Accept:'text/javascript' }}); return false;"
    new UnprefixedAttribute("href", ajax, attr)
  }
  
  
  private def processParameters(r: RequestState) {
    r.paramNames.filter{n => messageCallback.contains(n)}.foreach{
      n => 
    	val v = messageCallback(n)
      v.target !? (100L, ActionMessage(v.name, r.params(n), self, Some(this), r))
    }
    // messageCallback = TreeMap.Empty[String, ActionMessage]
  }
}

case class AjaxRerender(timeOut: long, sendTo: Actor, state: RequestState)
