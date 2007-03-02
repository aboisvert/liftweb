package net.liftweb.proto

import net.liftweb.mapper._
import scala.xml.Elem


class MappedTextarea[T<:Mapper[T]](owner : T) extends MappedString[T](owner) {
  
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
