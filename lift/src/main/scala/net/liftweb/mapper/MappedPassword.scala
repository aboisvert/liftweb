package net.liftweb.mapper

/*                                                *\
 (c) 2006 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util.FatLazy
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import scala.xml.{Node, Text, NodeSeq}
import java.util.Date
import net.liftweb.http.S
import net.liftweb.http.S._
import net.liftweb.util._

object MappedPassword {
  val blankPw = "*******"
}

class MappedPassword[T<:Mapper[T]](val fieldOwner: T) extends MappedField[String, T] {
  override def dbColumnCount = 2
  def dbFieldClass = classOf[String]
  
  override def dbColumnNames(in : String) = in.toLowerCase+"_pw" :: in.toLowerCase+"_slt" :: Nil

  def salt = this.salt_i
  
  private var password = FatLazy(defaultValue)
  private val salt_i = FatLazy(Safe.randomString(16))
  private var invalidPw = false
  private var invalidMsg = ""
  
  protected def real_i_set_!(value : String) : String = {
    password() = value match {
      case "*" | null | MappedPassword.blankPw if (value.length < 3) => {invalidPw = true ; invalidMsg = S.??("password.must.be.set") ; "*"}
      case MappedPassword.blankPw => {return "*"}
      case _ if (value.length > 4) => {invalidPw = false; hash("{"+value+"} salt={"+salt_i.get+"}")}
      case _ => {invalidPw = true ; invalidMsg = S.??("password.too.short"); "*"}
    }
    this.dirty_?( true)
    "*"
  }
  
  def setList(in: List[String]): Boolean =
    in match {
    case x1 :: x2 :: Nil if x1 == x2 => this.set(x1) ; true
    case _ => invalidPw = true; invalidMsg = S.??("passwords.do.not.match"); false
    }
  
  
  override def setFromAny(f: Any): String = {
    f match {
      case a : Array[String] if (a.length == 2 && a(0) == a(1)) => {this.set(a(0))}
      case l : List[String] if (l.length == 2 && l.head == l(1)) => {this.set(l.head)}
      case _ => {invalidPw = true; invalidMsg = S.??("passwords.do.not.match")}
    }
    is
  }

  override def renderJs_? = false
   
  def asJsExp = throw new NullPointerException("No way")
  
  def match_?(toMatch : String) = {
    hash("{"+toMatch+"} salt={"+salt_i.get+"}") == password.get
  }
  
  override def validate : List[ValidationIssue] = {
    if (!invalidPw && password.get != "*") Nil
    else if (invalidPw) List(ValidationIssue(this, invalidMsg))
    else List(ValidationIssue(this, S.??("password.must.set")))
  }
  
  def real_convertToJDBCFriendly(value: String): Object = hash("{"+value+"} salt={"+salt_i.get+"}")
  
  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.VARCHAR
  
  def defaultValue = "*"

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = MappedPassword.blankPw
  protected def i_was_! = MappedPassword.blankPw
  /**
    * Called after the field is saved to the database
    */
  override protected[mapper] def doneWithSave() {
  }
  
  protected def i_obscure_!(in : String) : String = in
  
  /**
   * Create an input field for the item
   */
  override def _toForm: Can[NodeSeq] = {
    val funcName = S.mapFunc({s: List[String] => this.setFromAny(s)})
    Full(<span><input type='password' name={funcName} 
	 value={is.toString}/>&nbsp;{S.??("repeat")}&nbsp;<input 
	 type='password' name={funcName}
	 value={is.toString}/></span>)
    }
    
  
  def jdbcFriendly(columnName : String) = {
    if (columnName.endsWith("_slt")) {
      salt_i.get
    } else if (columnName.endsWith("_pw")) {
      password.get
    } else {
      null
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (T, long, boolean) => unit = {
    if (columnName.endsWith("_slt")) {
      {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = if (isNull) null else v.toString}}
    } else if (columnName.endsWith("_pw")) {
      {(inst : T, v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = if (isNull) null else v.toString}}      
    } else {
      null
    }
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (T, String) => unit  = {
    if (columnName.endsWith("_slt")) {
      {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = v}}
    } else if (columnName.endsWith("_pw")) {
      {(inst : T, v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = v}}      
    } else {
      null
    }
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => unit   = {
    null
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, boolean, boolean) => unit   = {
    null
  }
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (T, AnyRef) => unit = {
    if (columnName.endsWith("_slt")) {
      inst match {
	case null => {(inst : T, v : AnyRef) => {}}
	case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.salt_i() = (if (v == null) null else v.toString); tv.resetDirty}}
      }
    } else if (columnName.endsWith("_pw")) {
      inst match {
	case null => {(inst : T, v : AnyRef) => {}}
	case _ => {(inst : T, v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedPassword[T]]; tv.password() = (if (v == null) null else v.toString); tv.resetDirty}}
      }
      
    } else {
      null
    }
  }
  
  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = if (colName.endsWith("_pw")) colName+" VARCHAR(48)" else colName+" VARCHAR(20)"
}
