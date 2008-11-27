package net.liftweb.record.field

import net.liftweb.http.{S, FieldError}
import net.liftweb.util._
import scala.xml._
import S._
import Helpers._

trait NumericField[MyType, OwnerType <: Record[OwnerType]] extends Field[MyType, OwnerType] {

  private def elem = <input type="text" name={S.mapFunc{s: List[String] => {
      this.setFromAny(s) match {
        case Empty => valueCouldNotBeSet = true
        case _ =>
      }}}} value={value.toString}
      tabindex={tabIndex toString}/>

  /**
   * Returns form input of this field
   */
  def toForm = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>
         <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem

    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }

  override def noValueErrorMessage = S.??("number.required")
}
