package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */

import net.liftweb.mapper.{Mapper, MappedField}
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Lazy
import net.liftweb.util.Lazy._
import java.util.Date

class MappedString[T](val owner : Mapper[T]) extends MappedField[String, T] {
  private val data : Lazy[String] =  Lazy{defaultValue} // defaultValue
  
  protected def i_set_!(value : String) : String = {
    if (!data.defined_? || value != data.get) {
      data() = value
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

  protected def i_get_! = data.get

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
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = null}}
      case _ => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (v == null) null else v.toString}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (Mapper[T], long, boolean) => unit = {
    {(inst : Mapper[T], v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (isNull) null else v.toString}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (Mapper[T], String) => unit  = {
    {(inst : Mapper[T], v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = v}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (Mapper[T], Date) => unit   = {
    {(inst : Mapper[T], v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (v == null) null else v.toString}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (Mapper[T], boolean, boolean) => unit   = {
    {(inst : Mapper[T], v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (isNull) null else v.toString}}
  }
}