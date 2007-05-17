package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Lazy
import net.liftweb.util.Lazy._
import java.util.Date
import java.util.regex._

class MappedString[T<:Mapper[T]](val owner : T,val maxLen: int) extends MappedField[String, T] {
  private val data : Lazy[String] =  Lazy(defaultValue) // defaultValue
  
  final def toLower(in: String): String = in match {
    case null => null
    case s => s.toLowerCase
  }
  
  final def trim(in: String): String = in match {
    case null => null
    case s => s.trim
  }
  
  final def notNull(in: String): String = in match {
    case null => ""
    case s => s
  }

  
  protected def real_i_set_!(value : String) : String = {
    if (!data.defined_? || value != data.get) {
      data() = value
      this.dirty_?( true)
    }
    data.get
  }
  
  /**
  * Get the JDBC SQL Type for this field
  */
  def getTargetSQLType = Types.VARCHAR
  
  def defaultValue = ""

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_get_! = data.get

  protected def i_obscure_!(in : String) : String = {
    ""
  }
  
  def ::=(in : Any) : String = {
    in match {
      case (s: String) :: _ => this := s
      case null => this := null
      case s: String => this := s
      case Some(s: String) => this := s
      case None => this := null
      case o => this := o.toString
    }
    //     this := (if (f != null) f.toString else null)
  }
  
  
  def getJDBCFriendly(field : String) : Object = get
  
  def real_convertToJDBCFriendly(value: String): Object = value
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    inst match {
      case null => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = null}}
      case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (v == null) null else v.toString}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (isNull) null else v.toString}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = v}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    {(inst : T, v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (v == null) null else v.toString}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    {(inst : T, v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedString[T]]; tv.data() = if (isNull) null else v.toString}}
  }
  
  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: int, msg: String)(value: String): List[ValidationIssue] = 
    if ((value eq null) || value.length < len) List(ValidationIssue(this, msg))
    else Nil

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: int, msg: String)(value: String): List[ValidationIssue] = 
    if ((value ne null) && value.length > len) List(ValidationIssue(this, msg))
    else Nil

  /**
   * Make sure that the field is unique in the database
   */
  def valUnique(msg: String)(value: String): List[ValidationIssue] =
    owner.getSingleton.findAll(By(this, value)).
      filter(!_.comparePrimaryKeys(this.owner)).
      map(x =>ValidationIssue(this, msg))

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: String)(value: String): List[ValidationIssue] = pat.matcher(value).matches match {
    case true => Nil
    case false => List(ValidationIssue(this, msg))
  }

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLen+")"
  
}
