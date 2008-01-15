package net.liftweb.record

/*            
   * (c) 2007-2008 WorldWide Conferencing, LLC
   * Distributed under an Apache License
   * http://www.apache.org/licenses/LICENSE-2.0
   */

import net.liftweb._
import util._
import scala.xml._
     
trait Record[MyType <: Record[MyType]] {self: MyType =>
  def meta: MetaRecord[MyType]
  
  def safe_? : Boolean = true
}

trait KeyedRecord[MyType <: KeyedRecord[MyType, KeyType], KeyType] extends Record[MyType] {
  self: MyType =>
  
  def primaryKey: KeyField[KeyType, MyType]
}

trait MetaRecord[BaseRecord <: Record[BaseRecord]] { self: BaseRecord =>
  /**
    * Convert the name and value of a field to a String.  Override this method
    * to change its default behavior
    */
  def fieldToString(name: String, strValue: String) = name+"="+strValue
  
  def fieldToXHtml(name: String, strValue: NodeSeq) = <td>{name}</td><td>{strValue}</td>
  
  def mutable_? = true
      
  def createWithMutatedField[FieldType](original: BaseRecord, field: Field[FieldType, BaseRecord], newValue: FieldType): BaseRecord = null.asInstanceOf[BaseRecord] // FIXME   
}

trait KeyedMetaRecord[BaseRecord <: KeyedRecord[BaseRecord, KeyType], KeyType] extends MetaRecord[BaseRecord] { self: BaseRecord =>

}

/*
object TestRecord extends TestRecord with MetaRecord[TestRecord] {
  
}
*/
  
/*
class TestRecord extends Record[TestRecord] {
  def meta = TestRecord
  
  object longing extends LongField(this)
}
*/