package net.liftweb.mapper

import scala.xml.{NodeSeq}
import net.liftweb.http.S
import net.liftweb.http.S._

class MappedTextarea[T<:Mapper[T]](owner : T) extends MappedString[T](owner) {
  
  
  /**
     * Create an input field for the item
     */
    override def toForm : NodeSeq = {
       val funcName = S.mapFunction(name, {s: List[String] => this ::= s; true})
       <textarea name={funcName} rows={textareaRows.toString} cols={textareaCols.toString}>{get.toString}</textarea>
    }
  /*
   override def i : Elem = {
   <textarea name={S.ae({s => this ::= s(0)})} rows={textareaRows.toString} cols={textareaCols.toString}>{get.toString}</textarea>
   }*/
  
  override def toString = {
    val v = get
    if (v == null || v.length < 100) super.toString
    else {
      displayName +"="+v.substring(0,40)+" ... "+v.substring(v.length - 40)
    }
  }
  
  def textareaRows  = 8
  
  def textareaCols = 20

}
