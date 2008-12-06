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

package net.liftweb.record.field

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.{S, FieldError}
import _root_.java.util.Calendar
import Helpers._
import S._


class DateTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Calendar, OwnerType] {
  def owner = rec

  def this(rec: OwnerType, value: Calendar) = {
    this(rec)
    set(value)
  }

  /**
   * Sets the field value from an Any
   */
  def setFromAny(f : Any): Can[Calendar] = toDate(f).map(d => {
    val cal = Calendar.getInstance()
    cal.setTime(d)
    this.set(cal)
  })


  def setFromString(s: String) : Can[Calendar] = {
   try{
    val cal = Calendar.getInstance()
    cal.setTime(parseInternetDate(s))

     Full(set(cal));
   } catch {
     case e: Exception => Empty
   }
  }

  private def elem = <input type="text"
      name={S.mapFunc(SFuncHolder(this.setFromAny(_)))}
      value={value match {case null => "" case s: Calendar => toInternetDate(s.getTime)}}
      tabindex={tabIndex toString}/>;

  def toForm = {
    var el = elem
    uniqueFieldId match {
      case Full(id) =>
        <div id={id+"_holder"}><div><label for={id+"_field"}>{displayName}</label></div>{el % ("id" -> (id+"_field"))}<lift:msg id={id}/></div>
      case _ => <div>{el}</div>
    }

  }

  def asXHtml: NodeSeq = {
    var el = elem
    uniqueFieldId match {
      case Full(id) =>  el % ("id" -> (id+"_field"))
      case _ => el
    }
  }

  def defaultValue = Calendar.getInstance

}

import java.sql.{ResultSet, Types}
import net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBDateTimeField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends DateTimeField[OwnerType](rec)
  with JDBCFieldFlavor[_root_.java.sql.Date] {

  def targetSQLType = Types.TIMESTAMP

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) = value match {
    case null => null
    case d => new _root_.java.sql.Date(d.getTime.getTime)
  }
}
