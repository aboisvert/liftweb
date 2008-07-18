package net.liftweb.mapper

import scala.xml.{NodeSeq}
import net.liftweb.http.S
import net.liftweb.http.S._
import net.liftweb.util._

class MappedTextarea[T<:Mapper[T]](owner : T, maxLen: int) extends MappedString[T](owner, maxLen) {
  /**
   * Create an input field for the item
   */
  override def _toForm: Can[NodeSeq] = {
    val funcName = S.mapFunc({s: List[String] => this.setFromAny(s)})
    Full(<textarea name={funcName}
	 rows={textareaRows.toString}
	 cols={textareaCols.toString}>{is.toString}</textarea>)
  }

  override def toString = {
    val v = is
    if (v == null || v.length < 100) super.toString
    else {
      displayName +"="+v.substring(0,40)+" ... "+v.substring(v.length - 40)
    }
  }

  def textareaRows  = 8

  def textareaCols = 20

}
