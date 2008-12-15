/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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

package net.liftweb.http.jquery

import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import JE._
import _root_.scala.xml._

/**
 * This contains Html artifacts that are heavily relying on JQuery
 */
object JqSHtml {

  /**
   * Create an autocomplete form based on a sequence.
   */
  def autocompleteObj[T](options: Seq[(T, String)], default: Can[T],
                         onSubmit: T => Unit): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) = secureOptions(options, default, onSubmit)
    val defaultString = default.flatMap(d => options.find(_._1 == d).map(_._2))

    autocomplete_*(nonces, defaultString, defaultNonce, secureOnSubmit)
  }

  def autocomplete_*(options: Seq[(String, String)], default: Can[String],
                     defaultNonce: Can[String], onSubmit: AFuncHolder): Elem = {
    val id = Helpers.nextFuncName
    val hidden = mapFunc(onSubmit)
    val data = JsArray(options.map { case (nonce, name) =>
          JsObj("name" -> name, "nonce" -> nonce)} :_*)
    val autocompleteOptions = JsRaw("""{
      minChars: 0,
      matchContains: true,
      formatItem: function(row, i, max) { return row.name; },
    }""")
    val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+data.toJsCmd+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(dt.nonce);
        });
      });""")

    (<span>
        <head>
          <link rel="stylesheet" href="/classpath/jquery-autocomplete/jquery.autocomplete.css" type="text/css" />
          <script type="text/javascript" src="/classpath/jquery-autocomplete/jquery.autocomplete.js" />
          <script type="text/javascript">{Unparsed(onLoad.toJsCmd)}</script>
        </head>
        <input type="text" id={id} value={default.openOr("")} />
        <input type="hidden" name={hidden} id={hidden} value={defaultNonce.openOr("")} />
     </span>)
  }

  def autocomplete(start: String, options: (String, Int) => Seq[String],
                   onSubmit: String => Unit, attrs: (String, String)*): NodeSeq =
  {
    val f = (ignore: String) => {
      val q = S.param("q").openOr("")
      val limit = S.param("limit").flatMap(asInt).openOr(10)
      PlainTextResponse(options(q, limit).map(s => s+"|"+s).mkString("\n"))
    }
    val func = mapFunc(SFuncHolder(f))
    val what: String = S.contextPath + "/" + LiftRules.ajaxPath+"?"+func+"=foo"

    val id = Helpers.nextFuncName
    val hidden = mapFunc(SFuncHolder(onSubmit))

    val autocompleteOptions = JsRaw("""{
      minChars: 0,
      matchContains: true,
    }""")
    val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+what.encJs+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(dt.nonce);
        });
      });""")

    <span>
      <head>
        <link rel="stylesheet" href="/classpath/jquery-autocomplete/jquery.autocomplete.css" type="text/css" />
        <script type="text/javascript" src="/classpath/jquery-autocomplete/jquery.autocomplete.js" />
        <script type="text/javascript">{Unparsed(onLoad.toJsCmd)}</script>
      </head>
    {
      attrs.foldLeft(<input type="text" id={id} value={start} />)(_ % _)
    }
      <input type="hidden" name={hidden} id={hidden} value={start} />
    </span>
  }
}
