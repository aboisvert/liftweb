package net.liftweb.record

/*            
 * (c) 2007-2008 WorldWide Conferencing, LLC
 * Distributed under an Apache License
 * http://www.apache.org/licenses/LICENSE-2.0
 */
   
import net.liftweb.util._
import scala.xml._
   
trait FieldHandler

trait FieldHandlerRequest[RetType <: FieldHandler]
   
object JdbcHandler extends JdbcFieldHandler
class JdbcFieldHandler extends FieldHandler
object XmlHandler extends XmlHandlerClass
class XmlHandlerClass extends FieldHandler
object JdbcRequest extends FieldHandlerRequest[JdbcFieldHandler]
object XmlRequest extends FieldHandlerRequest[XmlHandlerClass]

trait FieldLocator { self: SimpleField =>
  def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] = Empty
}

trait JdbcLocator extends FieldLocator { self: SimpleField =>
override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] = 
  request match {
    case JdbcRequest => Full(JdbcHandler)
    case _ => super.locateFieldHandler(request)
  }
}

trait XmlLocator extends FieldLocator { self: SimpleField =>
override def locateFieldHandler[T <: FieldHandler](request: FieldHandlerRequest[T]): Can[T] = 
  request match {
    case XmlRequest => Full(XmlHandler)
    case _ => super.locateFieldHandler(request)
  }
}

/**
  * A simple field that can store and retreive a value of a given type
  */
trait SimpleField extends FieldLocator {
  type SMyType
  type SOwnerType <: Record[SOwnerType]

  private[this] var data: SMyType = _
  private[this] var needsDefault = true
      
  /**
    * The default value of the field
    */
  def defaultValue: SMyType
  
  /**
    * The Record that owns this field
    */ 
  def owner: SOwnerType
  
  /**
    * The text name of this field
    */
  def name: String = _name
  
  private[record] var _name: String = _
  
  /**
    * Convert the field to a String... usually of the form "displayName=value"
    */
  def asString = owner.meta.fieldToString(displayName, toString)
  
  /**
    * Convert the field a XHTML... by default in the form &lt;td&gt;displayName&lt;td&gt;&lt;/td&gt;value&lt;/td&gt;
    */
  def asXHtml = owner.meta.fieldToXHtml(displayName, toXHtml)
  
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
      
  def fromString(in: String): Can[SMyType]
      
  def canWrite_? = owner.safe_? || checkCanWrite_?
      
  def checkCanWrite_? = true
  
  private var obscured: SMyType = _
   
  def obscure(in: SMyType): SMyType = obscured
  
  def set(in: SMyType): Unit = synchronized {
    if (checkCanWrite_?) {
    data = in 
    needsDefault = false
    }
    }
  def value: SMyType = synchronized{
    if (needsDefault) {data = defaultValue ; needsDefault = false} 

    if (canRead_?) data
    else obscure(data)
  }
  
  override def toString = value match {
    case null => "null"
    case s => s.toString
  }
  
  def toForm: NodeSeq
}

trait Field[MyType, OwnerType <: Record[OwnerType]] extends SimpleField {
  type SMyType = MyType
  type SOwnerType = OwnerType

  def apply(in: SMyType): OwnerType = if (owner.meta.mutable_?) {
    this.set(in)
    owner
  } else {
    owner.meta.createWithMutatedField(owner, this, in)
  }

}

trait KeyField[MyType, OwnerType <: KeyedRecord[OwnerType, MyType]] extends Field[MyType, OwnerType]

abstract class LongFieldProto[OwnerType <: Record[OwnerType]](val owner: OwnerType) extends Field[Long, OwnerType] with JdbcLocator with XmlLocator {
  def defaultValue = 0
  def fromString(in: String) = Full(Helpers.toLong(in))
}

