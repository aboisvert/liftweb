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
import net.liftweb.http.S
import S._

trait Mapper[A<:Mapper[A]] {
  private val secure_# = Safe.next
  private var was_deleted_? = false
  private var dbConnectionIdentifier:Option[ConnectionIdentifier] = None
  
  def getSingleton : MetaMapper[A];
  final def safe_? : boolean = {
    Safe.safe_?(secure_#)
  }
  
  implicit def thisToMappee(in: Mapper[A]): A = this.asInstanceOf[A]
  
  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }
  
  def connectionIdentifier(id: ConnectionIdentifier): A = {
    if (id != getSingleton.dbDefaultConnectionIdentifier || dbConnectionIdentifier.isDefined) dbConnectionIdentifier = Some(id)
    thisToMappee(this)
  }
  
  def connectionIdentifier = dbConnectionIdentifier getOrElse getSingleton.dbDefaultConnectionIdentifier
  
  /*
  def onFormPost(f : (A) => boolean) : A = {
    this
  }*/
  
  /**
    * Save the instance and return the instance
    */
  def saveMe: A = {
    this.save
    this
  }
  
  def save : boolean = {
    runSafe {
      getSingleton.save(this)
    }
  }
  
  def htmlLine : NodeSeq = {
    getSingleton.doHtmlLine(this)
  }
  
  def asHtml : NodeSeq = {
    getSingleton.asHtml(this)
  }
  
  def validate : List[ValidationIssue] = {
    runSafe {
      getSingleton.validate(this)
    }
  }
  
  /*
   def toForm : NodeSeq = {
   getSingleton.toForm(this)
   }
   */
  
  def delete_! : boolean = {
    if (!db_can_delete_?) false else
      runSafe {
	was_deleted_? = getSingleton.delete_!(this)
	was_deleted_?
      }
  }
  
  def toForm(f : (List[String]) => boolean): NodeSeq = {
    getSingleton.toForm(this) ++ <input type='hidden' name={S.mapFunction("submit", f)} value="n/a" />
  }
  
  def saved_? : boolean = getSingleton.saved_?(this)
  
  def db_can_delete_? : boolean = {
    getSingleton.saved_?(this) && !was_deleted_?
  }

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
  
  def ^ : A = this
  
  def checkNames : unit = {
    runSafe {
      if ((getSingleton ne null)) {
        getSingleton.checkFieldNames(this)
      }
    }
  }
  
  def comparePrimaryKeys(other: A) = false
}

object Mapper {
  implicit def fromSome[T<:Mapper[T]](in : Option[T]) : T = in.get
}

trait KeyedMapper[KeyType, OwnerType<:KeyedMapper[KeyType, OwnerType]] extends Mapper[OwnerType] {
  def primaryKeyField: MappedField[KeyType, OwnerType];
  
  override def comparePrimaryKeys(other: OwnerType) = primaryKeyField.get == other.primaryKeyField.get
                                   
  def reload: OwnerType = getSingleton.find(By(primaryKeyField, primaryKeyField.get)) getOrElse this    
}

