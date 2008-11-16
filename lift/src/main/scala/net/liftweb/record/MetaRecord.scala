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

import net.liftweb._
import util._
import scala.collection.mutable.{ListBuffer}
import scala.xml._
import net.liftweb.http.js.{JsExp, JE}
import net.liftweb.http.{FieldError, SHtml}
import net.liftweb.mapper.{Safe, KeyObfuscator}
import java.lang.reflect.Method
import field._

/**
 * Holds meta information and operations on a record
 */
trait MetaRecord[BaseRecord <: Record[BaseRecord]] { self: BaseRecord =>

  private[record] var fieldList: List[FieldHolder[BaseRecord]] = Nil

  private[record] var lifecycleCallbacks: List[(String, Method)] = Nil

  protected val rootClass = this.getClass.getSuperclass

  private def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
  private def isMappedField(m: Method) = classOf[Field[Nothing, BaseRecord]].isAssignableFrom(m.getReturnType)
  private def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)

  def introspect(rec: BaseRecord, methods: Array[Method])(f: (Method, Field[_, BaseRecord]) => Any) = {
    for (v <- methods  if isMagicObject(v) && isMappedField(v)) {
      v.invoke(rec, null) match {
        case mf: Field[_, BaseRecord] if !mf.ignoreField_? =>
          mf.setName_!(v.getName)
          f(v, mf)
        case _ =>
      }
    }

  }

  this.runSafe {
    val tArray = new ListBuffer[FieldHolder[BaseRecord]]

    lifecycleCallbacks = for (v <- this.getClass.getSuperclass.getMethods.toList
                              if isMagicObject(v) && isLifecycle(v)) yield (v.getName, v)

    introspect(this, this.getClass.getSuperclass.getMethods) {
      case (v, mf) => tArray += FieldHolder(mf.name, v, mf)
    }

    def findPos(in: AnyRef) : Can[Int] = {
      tArray.toList.zipWithIndex.filter(mft => in eq mft._1.field) match {
        case Nil => Empty
        case x :: xs => Full(x._2)
      }
    }

    val resArray = new ListBuffer[FieldHolder[BaseRecord]];

    fieldOrder.foreach(f => findPos(f).foreach(pos => resArray += tArray.remove(pos)))

    tArray.foreach(mft => resArray += mft)

    fieldList = resArray.toList
  }

  /**
   * Specifies if this Record is mutable or not
   */
  def mutable_? = true

  /**
   * Creates a mew record
   */
  def createRecord: BaseRecord = {
    val rec: BaseRecord = rootClass.newInstance.asInstanceOf[BaseRecord]
    rec.runSafe {
      introspect(rec, rec.getClass.getMethods) {case (v, mf) =>}
    }
    rec
  }

  /**
   * Creates a new record setting the value of the fields from the original object but
   * apply the new value for the specific field
   *
   * @param - original the initial record
   * @param - field the new mutated field
   * @param - the new value of the field
   */
  def createWithMutableField[FieldType](original: BaseRecord,
                                        field: Field[FieldType, BaseRecord],
                                        newValue: FieldType): BaseRecord = {
    val rec = createRecord

    for (f <- fieldList) {
      (f.name == field.name) match {
        case true => rec.fieldByName(f.name).map((recField: Field[Any, BaseRecord]) => recField.setFromAny(newValue) )
        case _ => rec.fieldByName(f.name).map((recField: Field[Any, BaseRecord]) =>
          original.fieldByName(f.name).map((m: Field[Any, BaseRecord]) => recField.setFromAny(m.value))
        )
      }
    }

    rec
  }

  /**
   * Returns the HTML representation of inst Record.
   *
   * @param inst - th designated Record
   * @return a NodeSeq
   */
  def asHtml(inst: BaseRecord): NodeSeq = NodeSeq.Empty

  /**
   * Validates the inst Record by calling validators for each field
   *
   * @pram inst - the Record tobe validated
   * @return a List of FieldError. If this list is empty you can assume that record was validated successfully
   */
  def validate(inst: BaseRecord): List[FieldError] = {
    foreachCallback(inst, _.beforeValidation)
    try{
	    fieldList.flatMap(holder => inst.fieldByName(holder.name) match {
	      case Full(field) => if (!field.valueCouldNotBeSet) {
	        field.validators.flatMap(_(field.value).map(FieldError(field, _)))
	      } else {
	        FieldError(field, Text(field.errorMessage)) :: Nil
	      }
	      case _ => Nil
	    })
    } finally {
      foreachCallback(inst, _.afterValidation)
    }
  }

  private[record] def foreachCallback(inst: BaseRecord, f: LifecycleCallbacks => Any) {
    lifecycleCallbacks.foreach(m => f(m._2.invoke(inst, null).asInstanceOf[LifecycleCallbacks]))
  }

  /**
   * Retuns the JavaScript expression for inst Record
   *
   * @param inst - the designated Record
   * @return a JsExp
   */
  def asJs(inst: BaseRecord): JsExp = JE.JsObj(("$lift_class", JE.Str("temp"))) // TODO - implement this

  /**
   * Returns the XHTML representation of inst Record.
   *
   * @param inst - the record to be rendered
   * @return the XHTML content as a NodeSeq
   */
  def toForm(inst: BaseRecord): NodeSeq =
    fieldList.flatMap(holder =>
      inst.fieldByName(holder.name).map((field:Field[Any, BaseRecord]) =>
        field.toForm).openOr(NodeSeq.Empty) ++ Text("\n"))

  /**
   * Returns the XHTML representation of inst Record based of a provided template
   *
   * @param inst - the record to be rendered
   * @return the XHTML content as a NodeSeq
   */
  def toForm(inst: BaseRecord, template: NodeSeq): NodeSeq = {
    template match {
      case e @ <lift:field_label>{_*}</lift:field_label> => e.attribute("name") match{
        case Some(name) => inst.fieldByName(name.toString).map((field: Field[Any, BaseRecord]) => field.label).openOr(NodeSeq.Empty)
        case _ => NodeSeq.Empty
      }

      case e @ <lift:field>{_*}</lift:field> => e.attribute("name") match{
        case Some(name) => inst.fieldByName(name.toString).map((field: Field[Any, BaseRecord]) => field.asXHtml).openOr(NodeSeq.Empty)
        case _ => NodeSeq.Empty
      }

      case e @ <lift:field_msg>{_*}</lift:field_msg> => e.attribute("name") match{
        case Some(name) => inst.fieldByName(name.toString).map((field: Field[Any, BaseRecord]) => field.uniqueFieldId match {
          case Full(id) => <lift:msg id={id}/>
          case _ => NodeSeq.Empty
        }).openOr(NodeSeq.Empty)
        case _ => NodeSeq.Empty
      }

      case Elem(namespace, label, attrs, scp, ns @ _*) =>
        Elem(namespace, label, attrs, scp, toForm(inst, ns.flatMap(n => toForm(inst, n))):_* )

      case s : Seq[_] => s.flatMap(e => e match {
        case Elem(namespace, label, attrs, scp, ns @ _*) =>
        Elem(namespace, label, attrs, scp, toForm(inst, ns.flatMap(n => toForm(inst, n))):_* )
        case x => x
      })

    }
  }



  private[record] def ??(meth: Method, inst: BaseRecord) = meth.invoke(inst, null).asInstanceOf[Field[_, BaseRecord]]

  /**
   * Get a field by the field name
   * @param fieldName -- the name of the field to get
   * @param actual -- the instance to get the field on
   *
   * @return Can[The Field] (Empty if the field is not found)
   */
  def fieldByName[T](fieldName: String, inst: BaseRecord): Can[Field[T, BaseRecord]] = {
    Can(fieldList.find(f => f.name == fieldName)).map(holder => ??(holder.method, inst).asInstanceOf[Field[T, BaseRecord]])
  }

  /**
   * Defined the order of the fields in this record
   *
   * @return a List of Field
   */
  def fieldOrder: List[Field[_, BaseRecord]] = Nil

  case class FieldHolder[T](name: String, method: Method, field: Field[_, T])
}

trait LifecycleCallbacks {
  def beforeValidation {}
  def afterValidation {}

  def beforeSave {}
  def beforeCreate {}
  def beforeUpdate {}

  def afterSave {}
  def afterCreate {}
  def afterUpdate {}

  def beforeDelete {}
  def afterDelete {}
}
