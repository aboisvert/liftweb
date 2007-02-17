package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */

import net.liftweb.mapper.{Mapper, MappedField}
import java.sql.{ResultSet, Types}
import java.util.Date
import java.lang.reflect.Method
import net.liftweb.util.Helpers._

class MappedDateTime[T](val owner : Mapper[T]) extends MappedField[Date, T] {
  private var data : Date = defaultValue
  
  protected def i_set_!(value : Date) : Date = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  
  /**
  * Get the JDBC SQL Type for this field
  */
  def getTargetSQLType(field : String) = Types.TIMESTAMP
  
  def defaultValue = null
  // private val defaultValue_i = new Date

  override def write_permission_? = true

  protected def i_get_! = data

  protected def i_obscure_!(in : Date) : Date = {
    new Date(0L)
  }
  
  def ::=(f : Any) : Date = {
    this := toDate(f)
  }
  
  override def read_permission_? = true
  
  def getJDBCFriendly(field : String) : Object = get match {
    case null => null
    case d => new java.sql.Date(d.getTime)
  }
  
  def convertToJDBCFriendly(value: Date): Object = value
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (Mapper[T], AnyRef) => unit = {
    inst match {
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(null); tv.resetDirty}}
      case d : java.util.Date => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(v.asInstanceOf[Date]); tv.resetDirty}}
      case d : java.sql.Date => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(new Date(v.asInstanceOf[java.sql.Date].getTime)); tv.resetDirty}}
      case _ => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(toDate(v)); tv.resetDirty}}
    }
  }
}