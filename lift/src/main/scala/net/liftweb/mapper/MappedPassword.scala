package net.liftweb.mapper

/*                                                *\
 (c) 2006 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util.Lazy
import net.liftweb.util.Lazy._
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import scala.xml.{Node, Text, Elem}
import java.util.Date

class MappedPassword[T<:Mapper[T]](val owner : T) extends MappedField[String, T] {

  /*
   override def i : Elem = {
   val name = S.ae({s => this ::= s})
   <span><input type='password' name={name} value={get.toString}/>&nbsp;
   repeat<input type='password' name={name} value={get.toString}/></span>
   }
   */
  
  override def dbColumnCount = 2
  
  override def dbColumnNames(in : String) = in.toLowerCase+"_pw" :: in.toLowerCase+"_slt" :: Nil

  def salt = this.salt_i
  
  override def asHtml : Node = Text(asString)
  
  private var password = Lazy{defaultValue}
  private val salt_i = Lazy{Safe.randomString(16)}
  private var invalidPw = false
  private var invalidMsg = ""
  
  protected def i_set_!(value : String) : String = {
    password() = value match {
      case "*" | null | "*******" if (password.get.length < 3) => {invalidPw = true ; invalidMsg = "Password to short" ; "*"}
      case "*******" => {return "*"}
      case _ if (value.length > 4) => {invalidPw = false; hash("{"+value+"} salt={"+salt_i.get+"}")}
      case _ => {invalidPw = true ; invalidMsg = "Password to short"; "*"}
    }
    this.dirty_?( true)
    "*"
  }
  
  override def ::=(f : Any) : String = {
    f match {
      case a : Array[String] if (a.length == 2 && a(0) == a(1)) => {this := a(0)}
      case l : List[String] if (l.length == 2 && l(0) == l(1)) => {this := l(0)}
      case _ => {invalidPw = true; invalidMsg = "Passwords do not match"}
    }
    get
  }

  
  def match_?(toMatch : String) = {
    hash("{"+toMatch+"} salt={"+salt_i.get+"}") == password.get
  }
  
  override def validate : List[ValidationIssues[String, T]] = {
    if (!invalidPw && password.get != "*") Nil
    else if (invalidPw) List(ValidationIssues(this, invalidMsg))
    else List(ValidationIssues(this, "Password must be set"))
  }
  
  def convertToJDBCFriendly(value: String): Object = hash("{"+value+"} salt={"+salt_i.get+"}")
  
  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType = Types.VARCHAR
  
  def defaultValue = "*"

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_get_! = "*******"

  protected def i_obscure_!(in : String) : String = in
  
  
  def getJDBCFriendly(columnName : String) = {
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
