package net.liftweb.example.snippet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.http._
import S._
import js._
import JsCmds._
import textile._
import net.liftweb.util._
import Helpers._
import scala.xml. _

class Json {
  object json extends JSONHandler {
    def apply(in: Any): JsCmd = 
      SetHtml("json_result", in match {
	case JSONCmd("show", _, p: String, _) => Text(p)
	case JSONCmd("textile", _, p: String, _) => 
	  TextileParser.toHtml(p, Empty)
	case JSONCmd("count", _, p: String, _) => Text(p.length+" Characters")
	case x => <b>Problem... didn't handle JSON message {x}</b>
      })
  }
  
  def sample = {
    <span>
    {Script(info.is.open_!._2)}
    <textarea id="json_question" rows="8" cols="50"></textarea>
    <br />
    <select id="json_select">
    <option value="show">Show</option>
    <option value="textile">Show in Textile</option>
    <option value="count">Count Characters</option>
    </select>
    <br />
    <button onclick={json.call(E("json_select") >> Value,
			       E("json_question") >> Value)
		   }>Click Me</button>
    <div id="json_result"></div>
    </span>
  }
  
  def time = Text(timeNow.toString)
}
