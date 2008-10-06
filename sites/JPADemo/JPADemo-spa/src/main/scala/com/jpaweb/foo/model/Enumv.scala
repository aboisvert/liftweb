
package com.foo.jpaweb.model

/* adds a valueOf function, assumes name is defined
add optional description */
trait Enumv  {
  
  this: Enumeration =>

  private var nameDescriptionMap = scala.collection.mutable.Map[String, String]()

  /* store a name and description for forms */
  def Value(name: String, desc: String) : Value = {
    nameDescriptionMap += (name -> desc)
    new Val(name)
  }
  
    /* get description if it exists else name */
  def getDescriptionOrName(ev: this.Value) = {
    try {
      nameDescriptionMap(""+ev)
    } catch {
      case e: NoSuchElementException => ev.toString
    }
  }

  /* get name description pair list for forms */
  def getNameDescriptionList =  this.elements.toList.map(v => (v.toString, getDescriptionOrName(v) ) ).toList

  /* get the enum given a string */
  def valueOf(str: String) = this.elements.toList.filter(_.toString == str) match {
    case Nil => null
    case x => x.head
  }
}
