package net.liftweb.mapper

/*                                                *\
 (c) 2006 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.collection.mutable._
import java.lang.reflect.Method
import java.sql.{ResultSet, Types}
import scala.xml.{Elem, Node, NodeSeq}

trait Mapper[A] {
  private val secure_# = Safe.next
  private var was_deleted_? = false
  def getSingleton : MetaMapper[A];
  final def safe_? : boolean = {
    Safe.safe_?(secure_#)
  }
  
  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }
  
  def onFormPost(f : (A) => boolean) : Mapper[A] = {
    this
  }
  
  def save : boolean = {
    runSafe {
      getSingleton.save(this)
    }
  }
  
  def htmlLine : Seq[Node] = {
    getSingleton.doHtmlLine(this)
  }
  
  def asHtml : Seq[Node] = {
    getSingleton.asHtml(this)
  }
  
  def sws_validate : List[ValidationIssues[AnyRef, A]] = {
    runSafe {
    getSingleton.sws_validate(this)
    }
  }
  
  def generateInputTable : Seq[Node] = {
    getSingleton.generateInputTable(this)
  }
  
  def delete_! : boolean = {
    if (!db_can_delete_?) false else
      runSafe {
	was_deleted_? = getSingleton.delete_!(this)
	was_deleted_?
      }
  }
  
  def db_can_delete_? : boolean = {
    getSingleton.saved_?(this) && !was_deleted_?
  }
  
  /*
  def i(f : (Array[String]) => unit) : Elem = {
    <input type='hidden' name={S.ae(f)} value="na"/>
  }
  
  def a(f : (Array[String]) => unit) : String = {
    S.ae(f)
  }
  */
  
  def dirty_? : boolean = getSingleton.dirty_?(this)
  
  override def toString = {
    val ret = new StringBuilder
    
    ret.append(this.getClass.getName)
    
    ret.append("={")
    
    ret.append(getSingleton.appendFieldToStrings(this))
    
    ret.append("}")
    
    ret.toString
  }
  
  def toXml: NodeSeq = {
    getSingleton.toXml(this)
  }
  
  def checkNames : unit = {
    runSafe {
      if ((getSingleton ne null)) {
        getSingleton.checkFieldNames(this)
      }
    }
  }
}

object Mapper {
  implicit def fromSome[T](in : Option[Mapper[T]]) : Mapper[T] = in.get
}

