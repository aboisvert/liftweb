package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import java.util.Date
import net.liftweb.util.FatLazy 

class MappedBinary[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Array[Byte], T] {
  private val data : FatLazy[Array[Byte]] =  FatLazy(defaultValue)
  private val orgData: FatLazy[Array[Byte]] = FatLazy(defaultValue)
  
  protected def real_i_set_!(value : Array[Byte]) : Array[Byte] = {
    data() = value
    this.dirty_?( true)
    value
  }
  
  def dbFieldClass = classOf[Array[Byte]]
  
  /**
   * Get the JDBC SQL Type for this field
   */
  //  def getTargetSQLType(field : String) = Types.BINARY
  def targetSQLType = Types.BINARY
  
  def defaultValue = null
  def maxLen = 1024
  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get
  
  protected def i_was_! = orgData.get
  
  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  protected def i_obscure_!(in : Array[Byte]) : Array[Byte] = {
    new Array[Byte](0)
  }
  
  override def setFromAny(f: Any): Array[Byte] =
    this.set((if (f == null) null
	     else if (f.isInstanceOf[Array[Byte]]) f.asInstanceOf[Array[Byte]];
	     else f.toString.getBytes("UTF-8")))
  
  def jdbcFriendly(field : String) : Object = is
  
  def real_convertToJDBCFriendly(value: Array[Byte]): Object = value
  
  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit = 
    (inst, v) => doField(inst, accessor, {case f: MappedBinary[T] =>
      val toSet = v match {
        case null => null
        case ba: Array[Byte] => ba
        case other => other.toString.getBytes("UTF-8")
      }
      f.data() = toSet
      f.orgData() = toSet
    })

  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit = null
  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit  = null
  def buildSetDateValue(accessor : Method, columnName : String): (T, Date) => Unit = null 
  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit = null
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "MEDIUMBLOB" // VARBINARY("+maxLen+")"
    case DerbyDriver => "LONG VARCHAR FOR BIT DATA"
  })
}
