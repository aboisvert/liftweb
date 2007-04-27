package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.util.Date
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import net.liftweb.util.Lazy
import net.liftweb.util.Lazy._

class MappedDateTime[T<:Mapper[T]](val owner : T) extends MappedField[Date, T] {
  private var data : Lazy[Date] = Lazy{defaultValue}
  
  protected def i_set_!(value : Date) : Date = {
    if (value != data) {
      data() = value
      this.dirty_?( true)
    }
    data
  }
  
  def toLong: long = i_get_! match {
    case d: Date => d.getTime / 1000L
    case _ => 0L
  }
  
  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType = Types.TIMESTAMP
  
  def defaultValue: Date = null
  // private val defaultValue_i = new Date

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_get_! = data.get

  protected def i_obscure_!(in : Date) : Date = {
    new Date(0L)
  }
  
  def ::=(f : Any) : Date = {
    this := toDate(f)
  }
  
  
  def getJDBCFriendly(field : String) : Object = get match {
    case null => null
    case d => new java.sql.Date(d.getTime)
  }
  
  def convertToJDBCFriendly(value: Date): Object = if (value == null) null else new java.sql.Date(value.getTime)
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor); tv.set(null.asInstanceOf[java.util.Date]); tv.resetDirty}}
      case d : java.util.Date => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor); tv.set(v.asInstanceOf[Date]); tv.resetDirty}}
      case d : java.sql.Date => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor); tv.set(new Date(v.asInstanceOf[java.sql.Date].getTime)); tv.resetDirty}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor); tv.set(toDate(v)); tv.resetDirty}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedDateTime[T]]; tv.data() = if (isNull) null else new Date(v)}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedDateTime[T]]; tv.data() = toDate(v)}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedDateTime[T]]; tv.data() = v}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedDateTime[T]]; tv.data() = null}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "DATETIME"
    case DerbyDriver => "TIMESTAMP"
  })
}
