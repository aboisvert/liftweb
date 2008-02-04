package net.liftweb.example.comet

import net.liftweb.http._
import S._
import net.liftweb.util._
import scala.xml._

class AskName(initInfo: CometActorInitInfo) extends 
      CometActor(initInfo) {
  def defaultPrefix = "ask_name"
    
  def render = ajaxForm(<div>What is your username?</div> ++ text("",name => answer(name.trim)) ++ 
    <input type="submit" value="Enter"/>)
}
