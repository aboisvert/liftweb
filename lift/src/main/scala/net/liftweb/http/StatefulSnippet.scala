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
package net.liftweb.http

import _root_.net.liftweb._
import util._
import Helpers._
import _root_.scala.xml.{NodeSeq, Elem}

/**
 * The same StatefulSnippet instance is used across a given page rendering.
 * <br/>
 * If the StatefulSnippet is used to render a form, a hidden field is added to
 * the form that causes the same instance to be used on the page that is the
 * target of the form submission.
 * <br/>
 * If you want to keep the same snippet for a page rendered via a link (&lt;a
 * href...&gt;) use the StatefulSnippet.link method to create the link.  This will
 * cause the registerThisSnippet method to be called and the same instance will
 * be used on the target page.
 * <pre>
 * class CountGame extends StatefulSnippet {
 *  val dispatch: DispatchIt = {
 *    case "run" => run _
 *  }
 *
 *  def run(xhtml: NodeSeq): NodeSeq = {
 *    if (lastGuess == number) {
 *      bind("count", chooseTemplate("choose", "win", xhtml), "number" --> number, "count" --> count)
 *    } else {
 *      bind("count", chooseTemplate("choose", "guess", xhtml),
 *        "input" --> text("", guess _),
 *        "last" --> lastGuess.map(v => if (v < number) v+" is low" else v+"is high").openOr("Make first Guess")
 *      )
 *    }
 *
 *  private def guess(in: String) {
 *    count += 1
 *    lastGuess = Full(toInt(in))
 *  }
 *
 *  private val number = 1 + randomInt(100)
 *  private var lastGuess: Box[Int] = Empty
 *  private var count = 0
 *
 *}
 *</pre>
 */
 trait StatefulSnippet extends DispatchSnippet {
   private[http] var snippetName: String = ""

   def registerThisSnippet() = S.setSnippetForClass(snippetName, this);

   def unregisterThisSnippet() = S.unsetSnippetForClass(snippetName);

   /**
   * create an anchor tag around a body
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
   def link(to: String, func: () => Any, body: NodeSeq): Elem = SHtml.link(to, () => {registerThisSnippet; func()}, body)

   def redirectTo(where: String) = S.redirectTo(where, registerThisSnippet)
 }

 trait DispatchSnippet {
   type DispatchIt = PartialFunction[String, NodeSeq => NodeSeq]

   def dispatch: DispatchIt
 }
