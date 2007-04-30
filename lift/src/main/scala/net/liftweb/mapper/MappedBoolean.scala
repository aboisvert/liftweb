package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import java.lang.{Boolean, Integer}
import java.util.Date

class MappedBoolean[T<:Mapper[T]](val owner : T) extends MappedField[boolean, T] {
  private var data : Option[boolean] = Some(defaultValue)
  def defaultValue = false

  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType = Types.BOOLEAN

  protected def i_get_! = data match {case None => false; case Some(v) => v}
  
  protected def real_i_set_!(value : boolean) : boolean = {
    if (data != None || value != data.get) {
      data = Some(value)
      this.dirty_?( true)
    }
    value
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def real_convertToJDBCFriendly(value: boolean): Object = new java.lang.Integer(if (value) 1 else 0)
  
  def getJDBCFriendly(field : String) = data match {case None => null; case _ => new java.lang.Integer(if (get) 1 else 0)}

  def ::=(in : Any) : boolean = {
    in match {
      case b: boolean => this := b
      case (b: boolean) :: _ => this := b
      case Some(b: boolean) => this := b
      case None => this := false
      case (s: String) :: _ => this := toBoolean(s)
      case null => this := false
      case s: String => this := toBoolean(s)
      case o => this := toBoolean(o)
    }
  }

  protected def i_obscure_!(in : boolean) = false
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Some(false)}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Some(toBoolean(v))}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (isNull) None else Some(v != 0L)}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (v == null) None else Some(toBoolean(v))}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (v == null) None else Some(true)}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (isNull) None else Some(v)}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "BOOLEAN"
    case DerbyDriver => "SMALLINT"
  })
}

