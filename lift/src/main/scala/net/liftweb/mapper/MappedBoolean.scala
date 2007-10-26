package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
// import java.lang.{Integer}
import net.liftweb.http.S
import java.util.Date
import net.liftweb.util._

class MappedBoolean[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Boolean, T] {
  private var data : Can[Boolean] = Full(defaultValue)
  private var orgData: Can[Boolean] = Full(defaultValue)
  
  def defaultValue: Boolean = false

  def dbFieldClass = classOf[Boolean]
  
  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BOOLEAN

  protected def i_is_! = data openOr false
  protected def i_was_! = orgData openOr false
  protected[mapper] def doneWithSave() {orgData = data}
  
  protected def real_i_set_!(value : Boolean) : Boolean = {
    dirty_?(data.map(_ != value) openOr true)
    data = Full(value)
    value
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def real_convertToJDBCFriendly(value: Boolean): Object = new java.lang.Integer(if (value) 1 else 0)
  
  def jdbcFriendly(field : String) = data.map(v => new java.lang.Integer(if(v) 1 else 0)) openOr null

  override def setFromAny(in: Any): Boolean = {
    in match {
      case b: Boolean => this.set(b)
      case (b: Boolean) :: _ => this.set(b)
      case Some(b: Boolean) => this.set(b)
      case Full(b: Boolean) => this.set(b)
      case Empty | Failure(_, _, _) | None => this.set(false)
      case (s: String) :: _ => this.set(toBoolean(s))
      case null => this.set(false)
      case s: String => this.set(toBoolean(s))
      case o => this.set(toBoolean(o))
    }
  }

  protected def i_obscure_!(in : Boolean) = false
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => Unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Full(false)}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Full(toBoolean(v))}}
    }
  }
  
  private def allSet(in: Can[Boolean]) {
    this.data = in
    this.orgData = in
  }
  
  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (isNull) Empty else Full(v != 0L))})

  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit = 
    (inst, v) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (v == null) Empty else Full(toBoolean(v)))})    

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (v == null) Empty else Full(true))})

  def buildSetBooleanValue(accessor: Method, columnName : String) : (T, Boolean, Boolean) => Unit   = 
    (inst, v, isNull) => doField(inst, accessor, {case tv: MappedBoolean[T] => tv.allSet(if (isNull) Empty else Full(v))})

  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "BOOLEAN"
    case DerbyDriver => "SMALLINT"
  })
  

  
    /**
   * Create an input field for the item
   */
  override def toForm = S.checkbox(is,this(_))
}

