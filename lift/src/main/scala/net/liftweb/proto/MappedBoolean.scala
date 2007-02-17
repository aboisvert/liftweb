package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */

import net.liftweb.mapper.{Mapper, MappedField, IndexedField}
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import java.lang.Boolean

class MappedBoolean[T](val owner : Mapper[T]) extends MappedField[boolean, T] {
  private var data : boolean = defaultValue
  def defaultValue = false

  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType(field : String) = Types.BOOLEAN

  protected def i_get_! = data
  
  protected def i_set_!(value : boolean) : boolean = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def read_permission_? = true
  override def write_permission_? = true
  
  def convertToJDBCFriendly(value: boolean): Object = new Boolean(value)
      
      
  def getJDBCFriendly(field : String) = new Boolean(get)

  def ::=(f : Any) : boolean = {
    this := toBoolean(f)
  }
  protected def i_obscure_!(in : boolean) = false
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (Mapper[T], AnyRef) => unit = {
    inst match {
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(false); tv.resetDirty}}
      case _ => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor); tv.set(toBoolean(v)); tv.resetDirty}}
    }
  }
}

