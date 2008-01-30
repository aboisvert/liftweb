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
import JE._
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
    {Script(json.jsCmd)}
    <textarea id="json_question" rows="8" cols="50"></textarea>
    <br />
    <select id="json_select">
    <option value="show">Show</option>
    <option value="textile">Show in Textile</option>
    <option value="count">Count Characters</option>
    <option value="error">Show an error</option>
    </select>
    <br />
    <button onclick={json.call(ElemById("json_select") ! Value,
			       ElemById("json_question") ! Value).toJsCmd
		   }>Click Me</button>
    <div id="json_result"></div>
    </span>
  }
}
