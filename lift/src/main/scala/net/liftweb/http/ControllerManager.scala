package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.{Actor}
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
    
    case 'shutdown =>
    Console.println("Shutting down ctrl mgr")
    self.exit('Shutdown)
    loop
    
    case ('EXIT, controller: Actor, reason: Exception) => 
      reason.printStackTrace
      try {
        controller.start
      } catch {
        case e=> e.printStackTrace // FIXME do better logging and better error recovery
      }
      loop
    
    case s => Console.println("Controller manager got message "+s); loop
  }
  
  private def find(theType: Option[String],name: Option[String], factory: Option[String]): Option[ControllerActor] = {
    theType.flatMap {
      myType: String => 
	val lookFor = (myType, name, factory)
	  // look in the cache for the controller or try to build one
	  controllers.get(lookFor) orElse {
            // build it and if we get one, put it in the cache
	    searchFactoryForController(myType, factory).map{ctrl => controllers(lookFor) = ctrl; ctrl}
	  }
    }
  }
  
  private val controllers = new HashMap[(String, Option[String], Option[String]), ControllerActor]
  
  private def searchFactoryForController(contType: String, factory: Option[String]): Option[ControllerActor] = {
    findFactory(factory).flatMap{f => f.construct(contType)} orElse {findControllerByType(contType)}
  }
  
  private def findFactory(factory: Option[String]) : Option[ControllerFactory] = {
    for (val factName <- factory;
         val cls <- findClass(factName, buildPackage("factory") ::: ("lift.app.factory" :: Nil),
             {c : Class => classOf[ControllerFactory].isAssignableFrom(c)});
         val ret <- tryo{cls.newInstance.asInstanceOf[ControllerFactory]})
     	yield ret
  }

  
  private def findControllerByType(contType: String): Option[ControllerActor] = {
    findClass(contType, buildPackage("controller") ::: ("lift.app.controller" :: Nil), 
              {c : Class => classOf[ControllerActor].isAssignableFrom(c)}).flatMap{
		cls =>
		  tryo {
		    val ret = cls.newInstance.asInstanceOf[ControllerActor];
		    ret.start
		    ret.link(self)
		    ret
		  }
	      }
  }

  /*
   
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
case class AskFindController(theType: Option[String],name: Option[String], factory: Option[String]) extends ControllerManagerMessage
case class AnswerFoundController(controller: Option[ControllerActor]) extends ControllerManagerMessage
