package net.liftweb.example.snippet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._


class CountGame extends StatefulSnippet {
  val dispatch: DispatchIt = {
    case "run" => run _
  }
  
  def run(xhtml: NodeSeq): NodeSeq = if (lastGuess == number) 
    bind("count", chooseTemplate("choose", "win", xhtml), "number" --> number, "count" --> count)
  else
    bind("count", chooseTemplate("choose", "guess", xhtml), "input" --> text("", guess _),
        "last" --> lastGuess.map(v => if (v < number) v+" is low" else v+" is high").openOr("Make first Guess"))
  
  private def guess(in: String) {
    count += 1
    lastGuess = Full(toInt(in))
  }
    
  private val number = 1 + randomInt(100)
  private var lastGuess: Can[Int] = Empty
  private var count = 0
}
