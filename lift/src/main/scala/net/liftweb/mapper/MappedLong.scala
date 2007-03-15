package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import java.lang.Long
import java.util.Date

class MappedLongForeignKey[T<:Mapper[T],O<:KeyedMapper[FKType, O], FKType](owner: T, foreign: KeyedMetaMapper[FKType, O]) extends MappedLong[T](owner) with BaseForeignKey {
  def defined_? = i_get_! != defaultValue
  
  override def getJDBCFriendly(field : String) = if (defined_?) new Long(get) else null
  override def getJDBCFriendly = if (defined_?) new Long(get) else null
  def obj = if(defined_?) foreign.find(i_get_!) else None
  
  def dbKeyToTable = foreign
  def dbKeyToColumn = foreign.primaryKeyField

  override def dbIndexed_? = true
            
  override def dbForeignKey_? = true
}

class MappedLongIndex[T<:Mapper[T]](owner : T) extends MappedLong[T](owner) with IndexedField[long] {

  override def writePermission_? = false // not writable
  
  override def dbIndexed_? = true

  def defined_? = i_get_! != defaultValue
  override def dbPrimaryKey_? = true

  override def defaultValue = -1L
  
  override def dbIndexFieldIndicatesSaved_? = {i_get_! != defaultValue}
  
  def makeKeyJDBCFriendly(in : long) = new java.lang.Long(in)
  
  def convertKey(in : String) : Option[long] = {
    if (in eq null) None
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Some(Long.parseLong(what))
    } catch {
      case _ => {None}
    }
  }
  
  override def db_display_? = false
  
  def convertKey(in : long) : Option[long] = {
    if (in < 0L) None
    else Some(in)
  }
  
  def convertKey(in : int) : Option[long] = {
    if (in < 0) None
    else Some(in)
  }
  
  def convertKey(in : AnyRef) : Option[long] = {
    if ((in eq null) || (in eq None)) None
    try {
      convertKey(in.toString)
    } catch {
      case _ => {None}
    }                                         
  }
  
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "BIGINT NOT NULL AUTO_INCREMENT UNIQUE"
    case DerbyDriver => "BIGINT NOT NULL GENERATED ALWAYS AS IDENTITY"
  })
}


class MappedLong[T<:Mapper[T]](val owner : T) extends MappedField[long, T] {
  private var data : long = defaultValue
  def defaultValue = 0L

  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType = Types.INTEGER

  protected def i_get_! = data
  
  protected def i_set_!(value : long) : long = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def convertToJDBCFriendly(value: long): Object = new Long(value)
  
  
  def getJDBCFriendly(field : String) = new Long(get)
  override def getJDBCFriendly = new Long(get)

  def ::=(in : Any) : long = {
    in match {
      case n: long => this := n
      case n: Number => this := n.longValue
      case (n: Number) :: _ => this := n.longValue
      case Some(n: Number) => this := n.longValue
      case None => this := 0L
      case (s: String) :: _ => this := toLong(s)
      case null => this := 0L
      case s: String => this := toLong(s)
      case o => this := toLong(o)
    }
  }

  protected def i_obscure_!(in : long) = 0L
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = 0L}}
      case _ if (inst.isInstanceOf[Number]) => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = if (v == null) 0L else v.asInstanceOf[Number].longValue}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = tryn(Long.parseLong(v.toString))}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = v}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = tryn(Long.parseLong(v))}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = if (v == null) 0L else v.getTime}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = if (v && !isNull) 1L else 0L}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case _ => "BIGINT"
  })
}

