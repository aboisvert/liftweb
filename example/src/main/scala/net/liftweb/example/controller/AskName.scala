package net.liftweb.example.controller

import net.liftweb.http._
import scala.collection.immutable.TreeMap

class AskName extends ControllerActor {
  def render = {
    val inputName = uniqueId+"_name"

      S.addFunctionMap(inputName,{in:List[String] => answer(in.head, S.request); true})
    
    val ret = <lift:form method="post">
    <div>What is your username?</div>
    <input name={inputName} type="text" value=""/><input value="Enter" type="submit"/>
    </lift:form>

    ret
  }
}
