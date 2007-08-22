package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.{Actor, Exit}
import Actor._
import scala.collection.mutable.HashMap
import scala.xml.{Node, NodeSeq, Comment}
import net.liftweb.util._
import Helpers._

class ControllerManager extends Actor {
  def act = { this.trapExit = true ; loop }
  
  private def loop: unit = react {
    case AskFindController(theType, name, factory) => {
      reply(AnswerFoundController(find(theType, name, factory)))
      loop
    }
    
    case ShutDown =>
    Log.debug("Shutting down ctrl mgr")
    self.exit()
    loop
    
    case Exit( controller, reason: Exception) => 
      reason.printStackTrace
      try {
        controller.start
      } catch {
        case e=> e.printStackTrace // FIXME do better logging and better error recovery
      }
      loop
    
    case s => Log.debug("Controller manager got message "+s); loop
  }
  
  private def find(theType: Can[String],name: Can[String], factory: Can[String]): Can[ControllerActor] = {
    theType.flatMap {
      myType: String => 
	val lookFor = (myType, name, factory)
	  // look in the cache for the controller or try to build one
	  Can(controllers.get(lookFor)) or {
            // build it and if we get one, put it in the cache
	    searchFactoryForController(myType, factory).map{ctrl => controllers(lookFor) = ctrl;
            name.foreach(n => ctrl ! SetName(n))
            ctrl}
	  }
    }
  }
  
  private val controllers = new HashMap[(String, Can[String], Can[String]), ControllerActor]
  
  private def searchFactoryForController(contType: String, factory: Can[String]): Can[ControllerActor] = {
    findFactory(factory).flatMap{f => f.construct(contType)} or {findControllerByType(contType)}
  }
  
  private def findFactory(factory: Can[String]) : Can[ControllerFactory] = {
    for (factName <- factory;
         cls <- findClass(factName, buildPackage("factory") ::: ("lift.app.factory" :: Nil),
             {c : Class => classOf[ControllerFactory].isAssignableFrom(c)});
         ret <- tryo{cls.newInstance.asInstanceOf[ControllerFactory]})
     	yield ret
  }

  
  private def findControllerByType(contType: String): Can[ControllerActor] = {
    findClass(contType, buildPackage("controller") ::: ("lift.app.controller" :: Nil), 
              {c : Class => classOf[ControllerActor].isAssignableFrom(c)}).flatMap{
		cls =>
		  tryo {
		    val ret = cls.newInstance.asInstanceOf[ControllerActor];
		    ret.start
		    ret.link(this)
		    ret
		  }
	      }
  }

  /*
   
   private def findController(controllerType : Can[Seq[Node]], controllerName : Can[Seq[Node]], controllerFactory : Can[Seq[Node]], kids : NodeSeq) : NodeSeq = {
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
   (controller !? (600L, AskRender(globalState ++ localState.keys.map{k => {k, localState(k)}}))) match {
   case Some(view: AnswerRender) => updateCallbacks(view.messages) ; view.xml 
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
   */
}

abstract class ControllerManagerMessage
case class AskFindController(theType: Can[String],name: Can[String], factory: Can[String]) extends ControllerManagerMessage
case class AnswerFoundController(controller: Can[ControllerActor]) extends ControllerManagerMessage
