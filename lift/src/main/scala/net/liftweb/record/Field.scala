/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record

import net.liftweb.util._
import net.liftweb.http.{S, FieldIdentifier, FieldError}
import scala.xml._

/**
  * A simple field that can store and retreive a value of a given type
  */
trait Field[MyType, OwnerType <: Record[OwnerType]] extends FieldIdentifier {

  type ValidationFunction = MyType => Can[Node]

  def apply(in: MyType): OwnerType = if (owner.meta.mutable_?) {
    this.set(in)
    owner
  } else {
    owner.meta.createWithMutableField(owner, this, in)
  }


  private[record] var data: MyType = _
  private[record] var needsDefault = true
  private[record] var obscured: MyType = _
  private[record] var fieldName: String = _
  private[record] var valueCouldNotBeSet = false
  private[record] var dirty = false

  /**
   * Return the owner of this field
   */
  def owner: OwnerType

  protected def dirty_?(b: Boolean) = dirty = b

  def resetDirty {
    if (safe_?) dirty_?(false)
  }

  /**
   * Should the field be ignored by the OR Mapper?
   */
  def ignoreField_? = false

  /**
   * The default value of the field
   */
  def defaultValue: MyType

  /**
   * The text name of this field
   */
  def name: String = fieldName

  /**
   * Convert the field to a String... usually of the form "displayName=value"
   */
  def asString = displayName + "=" + data

  /**
   * Convert the field value to an XHTML representation
   */
  def toXHtml = Text(toString)

  /**
   * The display name of the field (by default, the 'internal' name of the field)
   */
  def displayName = name

  /**
   * Can the value of this field be read without obscuring the result?
   */
  def canRead_? = owner.safe_? || checkCanRead_?

  /**
   * If the owner is not in "safe" mode, check the current environment to see if
   * the field can be read
   */
  def checkCanRead_? = true

  def canWrite_? = owner.safe_? || checkCanWrite_?

  def checkCanWrite_? = true

  def obscure(in: MyType): MyType = obscured

  def set(in: MyType): MyType = synchronized {
    if (checkCanWrite_?) {
        data = set_!(in)
        valueCouldNotBeSet = false
        needsDefault = false
    } else {
        valueCouldNotBeSet = true
        needsDefault = false
    }
    data
  }

  protected def set_!(in: MyType) = runFilters(in, setFilter)

  /**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
  protected def setFilter: List[MyType => MyType] = Nil

  def runFilters(in: MyType, filter: List[MyType => MyType]): MyType = filter match {
    case Nil => in
    case x :: xs => runFilters(x(in), xs)
  }

  def setFromAny(in: Any): Can[MyType]

  def setFromString(s: String) : Can[MyType]

  def value: MyType = synchronized {
    if (needsDefault) {
      data = defaultValue;
      needsDefault = false
    }

    if (canRead_?) data
    else obscure(data)
  }

  override def toString = value match {
    case null => "null"
    case s => s.toString
  }

  def toForm: NodeSeq

  def asXHtml: NodeSeq

  /**
   * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
   */
  final def safe_? : Boolean = owner.safe_?

  /**
   * Set the name of this field
   */
  private[record] final def setName_!(newName : String) : String = {
    if (safe_?) fieldName = newName
    fieldName
  }

  /**
   * The error message used when the fiel value could not be set
   */
  def errorMessage : String = ""

  /**
   * Return a list of functions that will be subsequently called for validating this field.
   * Each function takes a field-type parameter and returns a Can[Node].
   *
   * The field values is valid if all validation functions return an Empty Can
   */
  def validators : List[ValidationFunction] = Nil

  def tabIndex: Int = 1

  override def uniqueFieldId: Can[String] = Full(name+"_id")

  def label: NodeSeq = uniqueFieldId match {
    case Full(id) =>  <label for={id+"_field"}>{displayName}</label>
    case _ => NodeSeq.Empty
  }

}

import java.sql.{ResultSet, Types}
import net.liftweb.mapper.{DriverType}

/**
 * Desribes common aspects related with JDBC
 */
trait JDBCFieldFlavor[MyType] {

  def jdbcFriendly(field : String) : MyType

  def targetSQLType : Int

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String
}

trait KeyField[MyType, OwnerType <: Record[OwnerType] with KeyedRecord[OwnerType, MyType]] extends Field[MyType, OwnerType] {
  def ===(other: KeyField[MyType, OwnerType]): Boolean = this.value == other.value
}

trait FieldHandler

trait FieldHandlerRequest[RetType <: FieldHandler]

object JdbcHandler extends JdbcFieldHandler
class JdbcFieldHandler extends FieldHandler
object XmlHandler extends XmlHandlerClass
class XmlHandlerClass extends FieldHandler
object JdbcRequest extends FieldHandlerRequest[JdbcFieldHandler]
object XmlRequest extends FieldHandlerRequest[XmlHandlerClass]

trait FieldLocator[MyType, OwnerType <: Record[OwnerType]] { self: Field[MyType, OwnerType] =>
  def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] = Empty
}

trait JdbcLocator[MyType, OwnerType <: Record[OwnerType]] extends FieldLocator[MyType, OwnerType] {
  self: Field[MyType, OwnerType] =>

  override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] =
  request match {
    case JdbcRequest => Full(JdbcHandler)
    case _ => super.locateFieldHandler(request)
  }
}

trait XmlLocator[MyType, OwnerType <: Record[OwnerType]] extends FieldLocator[MyType, OwnerType] {
  self: Field[MyType, OwnerType] =>

  override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] =
  request match {
    case XmlRequest => Full(XmlHandler)
    case _ => super.locateFieldHandler(request)
  }
}
