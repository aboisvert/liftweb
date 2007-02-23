package net.liftweb.example.controller

import net.liftweb.http._
import scala.collection.immutable.TreeMap

class AskName extends ControllerActor {
  def render = {
    val inputName = uniqueId+"_name"

    val ret = <div><lift:form method="POST">
    What is your username?
    <input name={inputName} type="text" value=""/><input value="Enter" type="submit"/>
    </lift:form></div>

    XmlAndMap(ret, TreeMap(inputName -> {in:List[String] => answer(in.head); false}))
  }
}
