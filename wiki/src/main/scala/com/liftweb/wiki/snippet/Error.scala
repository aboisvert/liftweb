package com.liftweb.wiki.snippet

import scala.xml._
import net.liftweb.http._

// TODO use the ErrorReport snippet provided by Lift
class Error {
  def render(xhtml: Group): NodeSeq = {
    <div>
      Errors:
      <ol>
        { for (val error <- S.errors) yield <li>{ error }</li> }
      </ol>
      Warnings:
      <ol>
        { for (val warning <- S.warnings) yield <li>{ warning }</li> }
      </ol>
      Notices:
      <ol>
        { for (val notice <- S.notices) yield <li>{ notice }</li> }
      </ol>
    </div>
  }
}
