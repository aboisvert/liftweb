package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import java.util.Date

class MappedIntIndex[T<:Mapper[T]](owner : T) extends MappedInt[T](owner) with IndexedField[int] {

  override def writePermission_? = false // not writable

  override def dbPrimaryKey_? = true
  
  override def defaultValue = -1
  
  def defined_? = i_get_! != defaultValue
  
  override def dbIndexFieldIndicatesSaved_? = {i_get_! != defaultValue}
  
  def makeKeyJDBCFriendly(in : int) = new Integer(in)
  
  def convertKey(in : String) : Option[int] = {
    if (in eq null) None
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Some(Integer.parseInt(what))
    } catch {
      case _ => {None}
    }
  }
  
  override def dbDisplay_? = false
  
  def convertKey(in : int) : Option[int] = {
    if (in < 0) None
    else Some(in)
  }
  
  def convertKey(in : long) : Option[int] = {
    if (in < 0 || in > Integer.MAX_VALUE) None
    else Some(in.asInstanceOf[int])
  }
  
  def convertKey(in : AnyRef) : Option[int] = {
    if ((in eq null) || (in eq None)) None
    try {
      convertKey(in.toString)
    } catch {
      case _ => {None}
    }                                         
  }
  
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "INTEGER NOT NULL AUTO_INCREMENT UNIQUE"
    case DerbyDriver => "INTEGER NOT NULL GENERATED ALWAYS AS IDENITY"
  })

}


class MappedInt[T<:Mapper[T]](val owner : T) extends MappedField[int, T] {
  private var data : int = defaultValue
  def defaultValue = 0

  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType = Types.INTEGER

  protected def i_get_! = data
  
  protected def i_set_!(value : int) : int = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def convertToJDBCFriendly(value: int): Object = new Integer(value)
  
  
  def getJDBCFriendly(field : String) = new Integer(get)

  def ::=(in : Any) : int = {
    in match {
      case n: int => this := n
      case n: Number => this := n.intValue
      case (n: Number) :: _ => this := n.intValue
      case Some(n: Number) => this := n.intValue
      case None => this := 0
      case (s: String) :: _ => this := toInt(s)
      case null => this := 0
      case s: String => this := toInt(s)
      case o => this := toInt(o)
    }
  }
  
  protected def i_obscure_!(in : int) = 0
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = 0;}}
      case _ if (inst.isInstanceOf[Number]) => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = if (v == null) 0 else v.asInstanceOf[Number].intValue}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = toInt(v.toString)}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = v.asInstanceOf[int]}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = toInt(v.toString)}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = if (v == null) 0 else v.getTime.asInstanceOf[int]}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = if (v && !isNull) 1 else 0}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case _ => "INTEGER"
  })
}

