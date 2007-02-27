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

class MappedBinary[T](val owner : Mapper[T]) extends MappedField[Array[byte], T] {
  private val data : Lazy[Array[byte]] =  Lazy{defaultValue} // defaultValue
  
  protected def i_set_!(value : Array[byte]) : Array[byte] = {
    data() = value
    this.dirty_?( true)
    value
  }
  
  /**
  * Get the JDBC SQL Type for this field
  */
  def getTargetSQLType(field : String) = Types.BINARY
  
  def defaultValue = null
  def maxLen = 1024
  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_get_! = data.get

  protected def i_obscure_!(in : Array[byte]) : Array[byte] = {
    new Array[byte](0)
  }
  
  def ::=(f : Any) : Array[byte] = {
    this := (if (f == null) null
    else if (f.isInstanceOf[Array[byte]]) f.asInstanceOf[Array[byte]];
    else f.toString.getBytes("UTF-8"))
  }
  
  
  def getJDBCFriendly(field : String) : Object = get
  
  def convertToJDBCFriendly(value: Array[byte]): Object = value
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (Mapper[T], AnyRef) => unit = {
    inst match {
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBinary[T]]; tv.data() = null}}
      case _ => {(inst : Mapper[T], f : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBinary[T]]; tv.data() = (if (f == null) null
          else if (f.isInstanceOf[Array[byte]]) f.asInstanceOf[Array[byte]];
          else f.toString.getBytes("UTF-8"))}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (Mapper[T], long, boolean) => unit = {
    null
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (Mapper[T], String) => unit  = {
    null
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (Mapper[T], Date) => unit   = {
    null
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (Mapper[T], boolean, boolean) => unit   = {
    null
  }
}