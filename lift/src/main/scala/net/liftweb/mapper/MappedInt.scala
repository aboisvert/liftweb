package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import net.liftweb.util._
import java.util.Date
import net.liftweb.http._
import scala.xml.NodeSeq

class MappedEnum[T<:Mapper[T], ENUM <: Enumeration](val owner: T, val enum: ENUM) extends MappedField[ENUM#Value, T] {
  private var data: ENUM#Value = defaultValue
  def defaultValue: ENUM#Value = enum(0)
  def dbFieldClass = classOf[List[ENUM#Value]]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  
  protected def real_i_set_!(value: ENUM#Value): ENUM#Value = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def real_convertToJDBCFriendly(value: ENUM#Value): Object = new java.lang.Integer(value.id)
  
 def toInt = i_is_!.id
  def fromInt(in: Int): ENUM#Value = enum(in)
  
  def jdbcFriendly(field: String) = new java.lang.Integer(toInt)
  override def jdbcFriendly = new java.lang.Integer(toInt)


  def ::=(in : Any): ENUM#Value = {
    in match {
      case n: Int => this := fromInt(n)
      case n: Long => this := fromInt(n.toInt)
      case n: Number => this := fromInt(n.intValue)
      case (n: Number) :: _ => this := fromInt(n.intValue)
      case Some(n: Number) => this := fromInt(n.intValue)
      case None => this := defaultValue
      case (s: String) :: _ => this := fromInt(Helpers.toInt(s))
      case vs: ENUM#Value => this := vs
      case null => this := defaultValue
      case s: String => this := fromInt(Helpers.toInt(s))
      case o => this := fromInt(Helpers.toInt(o))
    }
  }
  
  protected def i_obscure_!(in : ENUM#Value) = defaultValue
  
  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit = 
    data match {
      case null => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]].data = defaultValue
      case n : Number => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]].data = fromInt(if (v == null) 0 else v.asInstanceOf[Number].intValue)
      case _ => (inst : T, v : AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]].data = fromInt(Helpers.toInt(v.toString))
    }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, Long, Boolean) => Unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]]; tv.data = fromInt(v.toInt)}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => Unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]]; tv.data = fromInt(Helpers.toInt(v.toString))}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]]; tv.data = fromInt(if (v == null) 0 else v.getTime.toInt)}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnum[T, ENUM]]; tv.data = fromInt(if (v && !isNull) 1 else 0)}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case _ => "BIGINT"
  })  
  
    /**
   * Create an input field for the item
   */
  override def toForm : NodeSeq = 
    S.select(enum.elements.toList.map(a => (a.id.toString, a.toString)), Full(toInt.toString),v => this(fromInt(Helpers.toInt(v))))
}

class MappedIntIndex[T<:Mapper[T]](owner : T) extends MappedInt[T](owner) with IndexedField[Int] {

  override def writePermission_? = false // not writable

  override def dbPrimaryKey_? = true
  
  override def defaultValue = -1
  
  def defined_? = i_is_! != defaultValue
  
  override def dbIndexFieldIndicatesSaved_? = {i_is_! != defaultValue}
  
  def makeKeyJDBCFriendly(in : Int) = new java.lang.Integer(in)
  
  def convertKey(in : String) : Can[Int] = {
    if (in eq null) Empty
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Full(Integer.parseInt(what))
    } catch {
      case _ => Empty
    }
  }
  
  override def dbDisplay_? = false
  
  def convertKey(in : Int) : Can[Int] = {
    if (in < 0) Empty
    else Full(in)
  }
  
  def convertKey(in : Long) : Can[Int] = {
    if (in < 0 || in > Integer.MAX_VALUE) Empty
    else Full(in.asInstanceOf[Int])
  }
  
  def convertKey(in : AnyRef) : Can[Int] = {
    if ((in eq null) || (in eq None)) None
    try {
      convertKey(in.toString)
    } catch {
      case _ => Empty
    }                                         
  }
  
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "INTEGER NOT NULL AUTO_INCREMENT UNIQUE"
    case DerbyDriver => "INTEGER NOT NULL GENERATED ALWAYS AS IDENITY"
  })

}


class MappedInt[T<:Mapper[T]](val owner : T) extends MappedField[Int, T] {
  private var data : Int = defaultValue
  def defaultValue = 0
  def dbFieldClass = classOf[Int]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.INTEGER

  protected def i_is_! = data
  
  protected def real_i_set_!(value : int) : int = {
    if (value != data) {
      data = value
      this.dirty_?( true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def +(in: Int): Int = is + in
      
  def real_convertToJDBCFriendly(value: int): Object = new Integer(value)
  
  
  def jdbcFriendly(field : String) = new Integer(is)

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
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, Long, Boolean) => unit = {
    {(inst : T, v: Long, isNull: Boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = v.asInstanceOf[int]}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = toInt(v.toString)}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = if (v == null) 0 else v.getTime.asInstanceOf[int]}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => unit   = {
    {(inst : T, v: Boolean, isNull: Boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedInt[T]]; tv.data = if (v && !isNull) 1 else 0}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case _ => "INTEGER"
  })
}

