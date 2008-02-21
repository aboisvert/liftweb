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
package net.liftweb.example.snippet

import net.liftweb.example.model._
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import scala.xml.{NodeSeq, Text, Group}

/**
 * The Arc Challenge is Paul Graham's quest for web framework concision.
 *
 * http://www.paulgraham.com/arcchallenge.html
 *
 * This is one potential lift-based solution to it using StatefulSnippets. 
 * There are doubtless many other ways.
 *
 * @author: Steve Jenson
 */
class ArcChallenge extends StatefulSnippet {
  val dispatch: DispatchIt = {
    case "ask" if phrase.isEmpty =>
      xhtml => ask

    case "ask" if thinking.isEmpty =>
      xhtml => think

    case "ask" =>
     xhtml => answer
  }

  /**
   * Step 1: Type in a Phrase.
   */
  def ask = {
    <p>
    Say Anything:
    {S.text("", p => phrase = Full(p))}
    {S.submit("Submit", ignore => {})}
    </p>
  }

  /**
   * Step 2: Show a link that takes you to the Phrase you entered.
   */
  def think = {
    S.submit("Click here to see what you said", ignore => thinking = Full(true))
  }

  /**
   * Step 3: Show the phrase.
   */
  def answer = {
    <p>
    You said: {phrase.openOr("")}
    </p>
  }

  def setPhrase(n: String) {
    phrase = Full(n)
  }

  private var phrase: Can[String] = Empty
  private var thinking: Can[Boolean] = Empty
}
