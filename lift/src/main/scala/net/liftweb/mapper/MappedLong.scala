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
import scala.xml.{NodeSeq, Text, Unparsed}
import net.liftweb.http.S
import S._

class MappedLongForeignKey[T<:Mapper[T],O<:KeyedMapper[Long, O]](owner: T, foreign: => KeyedMetaMapper[Long, O]) 
   extends MappedLong[T](owner) with MappedForeignKey[Long,T,O] with BaseForeignKey {
  def defined_? = /*i_get_! != defaultValue &&*/ i_is_! > 0L
  
  override def jdbcFriendly(field : String) = if (defined_?) new java.lang.Long(i_is_!) else null
  override def jdbcFriendly = if (defined_?) new java.lang.Long(i_is_!) else null
  private val _obj = Lazy(if(defined_?) foreign.find(i_is_!) else None)
  def obj: Option[O] = _obj.get
  
  def dbKeyToTable: KeyedMetaMapper[Long, O] = foreign
  def dbKeyToColumn = dbKeyToTable.primaryKeyField

  override def dbIndexed_? = true
            
  override def dbForeignKey_? = true

      /**
      * Called when Schemifier adds a foreign key.  Return a function that will be called when Schemifier
      * is done with the schemification.
      */
    def dbAddedForeignKey: Option[() => unit] = None
    
    override def toString = if (defined_?) super.toString else "NULL"

      def :=(v : Option[O]) : Long = this.:=(v.map(_.primaryKeyField.is) getOrElse 0L)
   def :=(v : O) : Long = this.:=(v.primaryKeyField.is)
      
   def apply(v: Option[O]): T = this(v.map(_.primaryKeyField.is) getOrElse 0L)
   def apply(v: O): T = this(v.primaryKeyField.is)
   
   def +(in: Long): Long = is + in
   
   /**
      * Given the driver type, return the string required to create the column in the database
      */
    override def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
       case DerbyDriver => "BIGINT"
       case MySqlDriver => "BIGINT UNSIGNED"
     })  
        
}

class MappedLongIndex[T<:Mapper[T]](owner : T) extends MappedLong[T](owner) with IndexedField[Long] {

  override def writePermission_? = false // not writable
  
  override def dbIndexed_? = true

  def defined_? = i_is_! != defaultValue
  override def dbPrimaryKey_? = true

  override def defaultValue = -1L
  
  override def dbIndexFieldIndicatesSaved_? = {i_is_! != defaultValue}
  
  def makeKeyJDBCFriendly(in : Long) = new java.lang.Long(in)
  
  def convertKey(in : String): Option[Long] = {
    if (in eq null) None
    try {
      val what = if (in.startsWith(name + "=")) in.substring((name + "=").length) else in
      Some(toLong(what))
    } catch {
      case _ => {None}
    }
  }
  
  override def dbDisplay_? = false
  
  def convertKey(in : Long): Option[Long] = {
    if (in < 0L) None
    else Some(in)
  }
  
  def convertKey(in : Int): Option[Long] = {
    if (in < 0) None
    else Some(in)
  }
  
  def convertKey(in : AnyRef): Option[Long] = {
    if ((in eq null) || (in eq None)) None
    else tryo(convertKey(in.toString)) getOrElse None
  }
  
  override def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case MySqlDriver => "BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE KEY"
    case DerbyDriver => "BIGINT NOT NULL GENERATED ALWAYS AS IDENTITY"
  })

}

class MappedEnumList[T<:Mapper[T], ENUM <: Enumeration](val owner: T, val enum: ENUM) extends MappedField[List[ENUM#Value], T] {
  private var data: List[ENUM#Value] = defaultValue
  def defaultValue: List[ENUM#Value] = Nil
  def dbFieldClass = classOf[List[ENUM#Value]]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  
  protected def real_i_set_!(value: List[ENUM#Value]): List[ENUM#Value] = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def real_convertToJDBCFriendly(value: List[ENUM#Value]): Object = new java.lang.Long(Helpers.toLong(value))
  
  private def toLong: Long = {
    i_is_!.foldLeft(enum.Set64)((a,b) => a + b.asInstanceOf[enum.Value]).underlyingAsLong  
  }
  
  def fromLong(in: Long): List[ENUM#Value] = enum.Set64(in).toList
  
  def jdbcFriendly(field: String) = new java.lang.Long(toLong)
  override def jdbcFriendly = new java.lang.Long(toLong)


  def ::=(in : Any): List[ENUM#Value] = {
    in match {
      case n: Long => this := fromLong(n)
      case n: Number => this := fromLong(n.longValue)
      case (n: Number) :: _ => this := fromLong(n.longValue)
      case Some(n: Number) => this := fromLong(n.longValue)
      case None => this := Nil
      case (s: String) :: _ => this := fromLong(Helpers.toLong(s))
      case vs: List[ENUM#Value] => this := vs
      case null => this := Nil
      case s: String => this := fromLong(Helpers.toLong(s))
      case o => this := fromLong(Helpers.toLong(o))
    }
  }
  
  protected def i_obscure_!(in : List[ENUM#Value]) = Nil
  
  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit = 
    data match {
      case null => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]].data = Nil
      case n : Number => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]].data = fromLong(if (v == null) 0L else v.asInstanceOf[Number].longValue)
      case _ => (inst : T, v : AnyRef) => getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]].data = fromLong(Helpers.toLong(v.toString))
    }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, Long, Boolean) => Unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]]; tv.data = fromLong(v)}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => Unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]]; tv.data = fromLong(Helpers.toLong(v.toString))}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]]; tv.data = fromLong(if (v == null) 0L else v.getTime)}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedEnumList[T, ENUM]]; tv.data = fromLong(if (v && !isNull) 1L else 0L)}}
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
     S.checkbox[ENUM#Value](enum.elements.toList, is,this(_)).toForm
}


class MappedLong[T<:Mapper[T]](val owner : T) extends MappedField[Long, T] {
  private var data : Long = defaultValue
  def defaultValue: Long = 0L
  def dbFieldClass = classOf[Long]

  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_! = data
  
  protected def real_i_set_!(value : Long): Long = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def real_convertToJDBCFriendly(value: Long): Object = new java.lang.Long(value)
  
  
  def jdbcFriendly(field : String) = new java.lang.Long(i_is_!)
  override def jdbcFriendly = new java.lang.Long(i_is_!)

  def ::=(in : Any) : long = {
    in match {
      case n: Long => this := n
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

  protected def i_obscure_!(in : Long) = 0L
  
  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit = 
    data match {
      case null => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedLong[T]].data = 0L
      case n : Number => (inst: T, v: AnyRef) => getField(inst, accessor).asInstanceOf[MappedLong[T]].data = if (v == null) 0L else v.asInstanceOf[Number].longValue
      case _ => (inst : T, v : AnyRef) => getField(inst, accessor).asInstanceOf[MappedLong[T]].data = toLong(v.toString)
    }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, Long, Boolean) => Unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = v}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => Unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = toLong(v.toString)}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = if (v == null) 0L else v.getTime}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedLong[T]]; tv.data = if (v && !isNull) 1L else 0L}}
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" "+(dbType match {
    case _ => "BIGINT"
  })
}

