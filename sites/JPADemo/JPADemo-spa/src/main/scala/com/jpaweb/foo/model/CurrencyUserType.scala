
package com.foo.jpaweb.model

import java.io.Serializable
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Types

import org.hibernate.HibernateException
import org.hibernate.usertype.UserType

/**
 * Helper class to translate money amount for hibernate
 */
abstract class CurrencyUserType[CZ <: CurrencyZone](cz: CZ) extends UserType {

  type MyCurrency = CZ#Currency

  val SQL_TYPES = Array(Types.NUMERIC.asInstanceOf[Int])

  override def sqlTypes() = SQL_TYPES

  override def returnedClass = cz.CurrencyUnit.getClass

  override def equals(x: Object, y: Object): Boolean = {
    if (x == null || y == null) return false
    else return x == y
  }

  override def hashCode(x: Object) = x.hashCode

  override def nullSafeGet(resultSet: ResultSet, names: Array[String], owner: Object): Object = {
    val dollarVal = resultSet.getBigDecimal(names(0))
    if (resultSet.wasNull()) return cz.make(0)
    else return cz.make(new BigDecimal(dollarVal))
  }

  override def nullSafeSet(statement: PreparedStatement, value: Object, index: Int): Unit = {
    if (value == null) {
      statement.setNull(index, Types.NUMERIC)
    } else {
      val dollarVal = value.asInstanceOf[MyCurrency]
      statement.setBigDecimal(index, dollarVal.amount.bigDecimal)
    }
  }

  override def deepCopy(value: Object): Object = value

  override def isMutable() = false

  override def disassemble(value: Object) = value.asInstanceOf[Serializable]

  override def assemble(cached: Serializable, owner: Object): Serializable = cached

  override def replace(original: Object, target: Object, owner: Object) = original

}

