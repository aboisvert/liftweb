/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.snippet

import _root_.net.liftweb.http._
import S._
import SHtml._
import js._
import js.jquery._
import _root_.net.liftweb.http.jquery._
import JqJsCmds._
import JsCmds._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml. _

class Ajax {

  def sample = {
    var cnt = 0

    <span>
    {
      a(() => {cnt = cnt + 1; SetHtml("cnt_id", Text( cnt.toString))},
          <span>Click me to increase the count
     (currently <span id='cnt_id'>0</span>)</span>)
      // a link that does AJAX to increment a counter server-side
      // and displays the result on the client
    } <br />

    <div id="messages"></div>
    {
      val opts = (1 to 50).toList.map(i => (i.toString, i.toString))
      // build the options
      ajaxSelect(opts, Full(1.toString),
     v => DisplayMessage("messages",
             Text("You selected "+v) ++
             <span>&nbsp;From the select box</span>,
             5 seconds, 1 second))
    } <br />
    {
      ajaxText("", v => DisplayMessage("messages",
               Text("You entered some text: "+v),
               4 seconds, 1 second))
    } <br />
    {
      swappable(<span>Click to edit: <span id='the_text'></span></span>,
    ajaxText("",
       v => DisplayMessage("messages",
               Text("You entered some text: "+v),
               4 seconds, 1 second)
       & SetHtml("the_text", Text(v))))
    } <br />

    <textarea id="the_area" cols="50" rows="10"></textarea>
    <br />

    {
      val (name, je) =
	ajaxCall(JE.JsRaw("document.getElementById('the_area').value"),
		 text => DisplayMessage("messages",
					<pre>{text}</pre>,
					4 seconds, 200))

      <a href="javascript://" onclick={je.toJsCmd}>Enter text above and click me</a>
    }
    <br/>
    <br/>

    <div id="some_stuff"></div>
    {
      a(<span>Click here and the stuff above will get a message</span>){
        DisplayMessage("some_stuff",
            <lift:embed what="/templates-hidden/ajax"/>,
            5 seconds, 1 second)
      }
    }

    <br/>
    <br/>

    {
      ajaxForm(hidden(() => JsCmds.Alert("Test Passed")) ++ submit("Test Ajax Form", () => ()))
    } <br />

    An example of autocomplete with a server round trip to
    calculate the autocomplete list
      {
        JqSHtml.autocomplete("", buildQuery _, _ => ())
      }
      <br />
    </span>
  }

  private def buildQuery(current: String, limit: Int): Seq[String] = {
    Log.info("Checking on server side with "+current+" limit "+limit)
    (1 to limit).map(n => current+""+n)
  }

  def time = Text(timeNow.toString)
}
