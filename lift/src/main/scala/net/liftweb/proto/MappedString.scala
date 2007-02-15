package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */

import net.liftweb.mapper.{Mapper, MappedField}
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method

class MappedString[T](val owner : Mapper[T]) extends MappedField[String, T] {
  private var data : String = defaultValue
  
  protected def i_set_!(value : String) : String = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  
  /**
  * Get the JDBC SQL Type for this field
  */
  def getTargetSQLType(field : String) = Types.VARCHAR
  
  def defaultValue = ""
  def maxLen = 32
  override def write_permission_? = true

  protected def i_get_! = data

  protected def i_obscure_!(in : String) : String = {
    ""
  }
  
  def ::=(f : Any) : String = {
    this := (if (f != null) f.toString else null)
  }
  
  override def read_permission_? = true
  
  def getJDBCFriendly(field : String) : Object = get
  
  def convertToJDBCFriendly(value: String): Object = value
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (Mapper[T], AnyRef) => unit = {
    inst match {
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(null); tv.resetDirty}}
      case _ => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(if (v == null) null else v.toString); tv.resetDirty}}
    }
  }
}