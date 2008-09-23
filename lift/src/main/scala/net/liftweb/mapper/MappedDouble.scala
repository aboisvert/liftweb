package net.liftweb.mapper

import _root_.java.sql.{ResultSet, Types}
import _root_.java.lang.reflect.Method
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.util.Date
import _root_.net.liftweb.http._
import _root_.scala.xml.NodeSeq
import js._

class MappedDouble[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Double, T] {
	private var data: Double = defaultValue
	private var orgData: Double = defaultValue

	private def st(in: Double) {
		data = in
		orgData = in
	}

	def defaultValue: Double = 0.0
	def dbFieldClass = classOf[Double]

	protected def i_is_! = data
	protected def i_was_! = orgData

	override def doneWithSave() {
		orgData = data
	}

	def toDouble(in: Any): Double = {
		in match {
			case null => 0.0
			case i: Int => i
			case n: Long => n
			case n : Number => n.doubleValue
			case (n: Number) :: _ => n.doubleValue
			case Some(n) => toDouble(n)
			case None => 0.0
			case s: String => s.toDouble
			case x :: xs => toDouble(x)
			case o => toDouble(o.toString)
		}
	}

	override def readPermission_? = true
	override def writePermission_? = true

	protected def i_obscure_!(in : Double) = defaultValue

	protected def real_i_set_!(value : Double): Double = {
		if (value != data) {
			data = value
			dirty_?(true)
		}
		data
	}

	def asJsExp = JE.Num(is)

	override def setFromAny(in: Any): Double = {
		in match {
			case n: Double => this.set(n)
			case n: Number => this.set(n.doubleValue)
			case (n: Number) :: _ => this.set(n.doubleValue)
			case Some(n: Number) => this.set(n.doubleValue)
			case None => this.set(0.0)
			case (s: String) :: _ => this.set(toDouble(s))
			case null => this.set(0L)
			case s: String => this.set(toDouble(s))
			case o => this.set(toDouble(o))
		}
	}

	def real_convertToJDBCFriendly(value: Double): Object = new _root_.java.lang.Double(value)

	/**
	* Get the JDBC SQL Type for this field
	*/
	def targetSQLType = Types.DOUBLE
	def jdbcFriendly(field : String) = new _root_.java.lang.Double(i_is_!)
	def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null
	def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
		(inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(if (v == null) defaultValue else v.getTime)})

	def buildSetStringValue(accessor: Method, columnName: String): (T, String) =>
		Unit = (inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(toDouble(v))})

	def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) =>
		Unit = (inst, v, isNull) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(if (isNull) defaultValue else v)})

	def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) =>
		Unit = (inst, v) => doField(inst, accessor, {case f: MappedDouble[T] => f.st(toDouble(v))})

	def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.doubleColumnType
}

