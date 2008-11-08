
package com.foo.jpaweb.model

import java.io.Serializable
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Types

import org.hibernate.HibernateException
import org.hibernate.usertype.UserType

/**
 * Helper class to translate enum for hibernate
 */
abstract class EnumvType(val et: Enumeration with Enumv) extends UserType {

  val SQL_TYPES = Array({Types.VARCHAR})

  override def sqlTypes() = SQL_TYPES

  override def returnedClass = classOf[et.Value]

  override def equals(x: Object, y: Object): Boolean = {
    return x == y
  }

  override def hashCode(x: Object) = x.hashCode

  override def nullSafeGet(resultSet: ResultSet, names: Array[String], owner: Object): Object = {
    val value = resultSet.getString(names(0))
    if (resultSet.wasNull()) return null
    else {
      return et.valueOf(value)
    }
  }

  override def nullSafeSet(statement: PreparedStatement, value: Object, index: Int): Unit = {
    if (value == null) {
      statement.setNull(index, Types.VARCHAR)
    } else {
      val en = value.toString
      statement.setString(index, en)
    }
  }

  override def deepCopy(value: Object): Object = value

  override def isMutable() = false

  override def disassemble(value: Object) = value.asInstanceOf[Serializable]

  override def assemble(cached: Serializable, owner: Object): Serializable = cached

  override def replace(original: Object, target: Object, owner: Object) = original

}


