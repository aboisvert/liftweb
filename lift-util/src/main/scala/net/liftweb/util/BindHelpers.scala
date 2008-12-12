package net.liftweb.util

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

import _root_.scala.xml.{NodeSeq, Node, SpecialNode, Text, Elem,
                         Group, MetaData, Null, UnprefixedAttribute,
                         PrefixedAttribute}

trait Bindable {
  def asHtml: NodeSeq
}

object BindHelpers extends BindHelpers

/**
 * The helpers assocated with bindings
 */
trait BindHelpers {
  /**
   * Takes attributes from the first node of 'in' (if any) and mixes
   * them into 'out'. Curried form can be used to produce a
   * NodeSeq => NodeSeq for bind.
   *
   * @param in where to take the attributes from
   * @param out where to put the attributes
   *
   * @return 'out' element with attributes from 'in'
   */
  def mixinAttributes(out: Elem)(in: NodeSeq): NodeSeq = {
    val attributes = in.firstOption.map(_.attributes).getOrElse(Null)
    out % attributes
  }

  /**
   * Choose one of many templates from the children.  Looking for the
   * tag &lt;choose:stuff&gt; ... &lt;/choose:stuff&gt;
   *
   * @param prefix the prefix (e.g., "choose")
   * @param tag the tag to choose (e.g., "stuff")
   * @param xhtml the incoming node sequence
   *
   * @return the first matching node sequence
   */
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq = (xhtml \\ tag).toList.filter(_.prefix == prefix) match {
    case Nil => NodeSeq.Empty
    case x :: xs => x.child
  }

  /**
   * Choose one of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag: String): Can[NodeSeq] =
  (xhtml \\ tag).toList.filter(_.prefix == prefix) match {
    case Nil => Empty
    case x :: xs => Full(x.child)
  }

  /**
   * Choose two of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String): Can[(NodeSeq, NodeSeq)] =
  for (x1 <- template(xhtml, prefix, tag1);
       x2 <- template(xhtml, prefix, tag2)) yield (x1, x2)

  /**
   * Choose three of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String, tag3: String): Can[(NodeSeq, NodeSeq, NodeSeq)] =
  for (x1 <- template(xhtml, prefix, tag1);
       x2 <- template(xhtml, prefix, tag2);
       x3 <- template(xhtml, prefix, tag3)) yield (x1, x2, x3)

  /**
   * Base class for Bind parameters. A bind parameter has a name and is able to extract its value from a NodeSeq.
   */
  sealed trait BindParam {
    def name: String
    def calcValue(in: NodeSeq): NodeSeq
  }

  trait BindWithAttr {
    def newAttr: String
  }

  /**
   * Constant BindParam always returning the same value
   */
  case class TheBindParam(name: String, value: NodeSeq) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value
  }

  /**
   * Constant BindParam always returning the same value
   */
  case class TheStrBindParam(name: String, value: String) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value)
  }

  /**
   * BindParam taking its value from an attribute
   */
  case class AttrBindParam(name: String, myValue: NodeSeq,
                           newAttr: String) extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): NodeSeq = myValue
  }

  /**
   * BindParam using a function to calculate its value
   */
  case class FuncBindParam(name: String, value: NodeSeq => NodeSeq) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }
  /**
   * BindParam using a function to calculate its value
   */
  case class FuncAttrBindParam(name: String, value: NodeSeq => NodeSeq, newAttr: String) extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): NodeSeq = value(in)
  }

  case class OptionBindParam(name: String, value: Option[NodeSeq]) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value getOrElse NodeSeq.Empty
  }

  case class CanBindParam(name: String, value: Can[NodeSeq]) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = value openOr NodeSeq.Empty
  }

  case class SymbolBindParam(name: String, value: Symbol) extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.name)
  }

  case class IntBindParam(name: String, value: Int) extends Tuple2[String, Int](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

  case class LongBindParam(name: String, value: Long) extends Tuple2[String, Long](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

  case class BooleanBindParam(name: String, value: Boolean) extends Tuple2[String, Boolean](name, value) with BindParam {
    def calcValue(in: NodeSeq): NodeSeq = Text(value.toString)
  }

case class TheBindableBindParam[T <: Bindable](name: String, value: T) extends Tuple2[String, T](name, value) with BindParam {
  def calcValue(in: NodeSeq): NodeSeq = value.asHtml
}

/**
   * transforms a Can into a Text node
   */
  object BindParamAssoc {
    implicit def canStrCanNodeSeq(in: Can[Any]): Can[NodeSeq] = in.map(_ match {
        case null => Text("null")
        case v => Text(v.toString)
      })
  }

  private def snToNs(in: Seq[Node]): NodeSeq = in

  class SuperArrowAssoc(name: String) {
    // Because JsObj is a subclass of Node, we don't want it
    // getting caught because it's not a bind param
    def ->[T <: SpecialNode](in: T with SpecialNode) = Tuple2[String, T](name, in)

    def ->(in: String) = TheStrBindParam(name, in)
    // def ->[T](in: T)(implicit f: T => String) = TheStrBindParam(name, f(in))
    def ->(in: NodeSeq) = TheBindParam(name, in)
    def ->(in: Text) = TheBindParam(name, in)
    def ->(in: Node) = TheBindParam(name, in)
    def ->(in: Seq[Node]) = TheBindParam(name, in)
    def ->(in: NodeSeq => NodeSeq) = FuncBindParam(name, in)
    def ->(in: Can[NodeSeq]) = CanBindParam(name, in)
    def ->(in: Option[NodeSeq]) = OptionBindParam(name, in)
    def ->(in: Symbol) = SymbolBindParam(name, in)
    def ->(in: Int) = IntBindParam(name, in)
    def ->(in: Long) = LongBindParam(name, in)
    def ->(in: Boolean) = BooleanBindParam(name, in)
    def ->[T <: Bindable](in: T with Bindable) = TheBindableBindParam[T](name, in)
    def ->[T](in: T) = Tuple2[String, T](name, in)
  }

  implicit def strToSuperArrowAssoc(in: String): SuperArrowAssoc = new SuperArrowAssoc(in)

  /**
   * This class creates a BindParam from an input value
   *
   * @deprecated use -> instead
   */
  @deprecated
  class BindParamAssoc(val name: String) {
    def -->(value: String): BindParam = TheBindParam(name, Text(value))
    def -->(value: NodeSeq): BindParam = TheBindParam(name, value)
    def -->(value: Symbol): BindParam = TheBindParam(name, Text(value.name))
    def -->(value: Any): BindParam = TheBindParam(name, Text(if (value == null) "null" else value.toString))
    def -->(func: NodeSeq => NodeSeq): BindParam = FuncBindParam(name, func)
    def -->(value: Can[NodeSeq]): BindParam = TheBindParam(name, value.openOr(Text("Empty")))
  }

  /**
   * transforms a String to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>"David" --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated
  implicit def strToBPAssoc(in: String): BindParamAssoc = new BindParamAssoc(in)

  /**
   * transforms a Symbol to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>'David --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated
  implicit def symToBPAssoc(in: Symbol): BindParamAssoc = new BindParamAssoc(in.name)

  /**
   * This extractor is used to determine at runtime if a Function1[_, _] is
   * actually a NodeSeq => NodeSeq. This is a hack to get around JVM type
   * erasure.
   */
  /*
   object Function1NodeSeqToNodeSeq {
   def unapply[A, B](f: Function1[A, B]): Option[NodeSeq => NodeSeq] =
   if (f.getClass.getMethods.exists{ method =>
   lazy val params: Seq[Class[_]] = method.getParameterTypes
   method.getName == "apply" &&
   params.length == 1 &&
   params.exists(_.isAssignableFrom(classOf[NodeSeq])) &&
   classOf[NodeSeq].isAssignableFrom(method.getReturnType)
   }) Some(f.asInstanceOf[NodeSeq => NodeSeq])
   else None
   }*/

  /*
   /**
    * Transforms a Tuple2[String, _] to a BindParam
    */
   implicit def pairToBindParam[T](p: Tuple2[String, T]): BindParam = {
   val (name, value) = p
   value match {
   case v: String => TheBindParam(name, Text(v))
   case v: NodeSeq => TheBindParam(name, v)
   case v: Symbol => TheBindParam(name, Text(v.name))
   case fn: Function1[_, _] =>
   Function1NodeSeqToNodeSeq.unapply(fn) match {
   case Some(func) => FuncBindParam(name, func)
   case _ => TheBindParam(name, Text(fn.toString))
   }
   case v: Can[_] =>
   val ov = v openOr Text("Empty")
   ov match {
   case o: NodeSeq => TheBindParam(name, o)
   case _ => TheBindParam(name, Text(v.toString))
   }
   case _ => TheBindParam(name, Text(if (value == null) "null" else value.toString))
   }
   }
   */
  /**
   * Transforms a Tuple2[Symbol, _] to a BindParam
   */
  /*
   implicit def symbolPairToBindParam[T](p: Tuple2[Symbol, T]): BindParam =
   pairToBindParam((p._1.name, p._2))
   */
  /**
   * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform
   * function, which can be used to format the returned NodeSeq.
   *
   * @deprecated use bind instead
   */
  @deprecated
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node => node match {
          case s: Elem if (node.prefix == namespace) =>
            if (transform.isDefinedAt(node.label))
            transform(node.label)(node)
            else
            Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
          case Group(nodes) => Group(rec_xbind(nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, rec_xbind(node.child) : _*)
          case n => node
        }
      }
    }

    rec_xbind(xml)
  }

  /**
   * Bind a set of values to parameters and attributes in a block of XML.<p/>
   * Usage:<pre>
   *   bind("user", <user:hello>replace this</user:hello>, "hello" --> <h1/>) must ==/(<h1></h1>)
   * </pre>
   */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq =
  bind(namespace, Empty, Empty , xml, params :_*)

  /**
   * Bind a set of values to parameters and attributes in a block of XML.<p/>
   * Usage:<pre>
   *   bind("user", <user:hello>replace this</user:hello>, "hello" --> <h1/>) must ==/(<h1></h1>)
   * </pre>
   */
  def bind(namespace: String, nodeFailureXform: Can[NodeSeq => NodeSeq],
           paramFailureXform: Can[PrefixedAttribute => MetaData],
           xml: NodeSeq, params: BindParam*): NodeSeq = {
    val map: _root_.scala.collection.immutable.Map[String, BindParam] = _root_.scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))

    def attrBind(attr: MetaData): MetaData = attr match {
      case Null => Null
      case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
      case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
          case None => paramFailureXform.map(_(pa)) openOr new PrefixedAttribute(pa.pre, pa.key, Text("FIX"+"ME find to bind attribute"), attrBind(pa.next))
          case Some(abp: BindWithAttr) => new UnprefixedAttribute(abp.newAttr, abp.calcValue(pa.value), attrBind(pa.next))
          case Some(bp: BindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
        }
      case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
    }

    def in_bind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
        node match {
          case s : Elem if (node.prefix == namespace) => {
              map.get(node.label) match {
                case None =>
                  nodeFailureXform.map(_(s)) openOr s

                case Some(ns) => ns.calcValue(s.child)
              }
            }
          case Group(nodes) => Group(in_bind(nodes))
          case s : Elem => Elem(node.prefix, node.label, attrBind(node.attributes), node.scope, in_bind(node.child) : _*)
          case n => node
        }
      }
    }
    in_bind(xml)
  }

  /**
   * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
   * according to the value of the "name" attribute.<p/>
   * Usage: <pre>
   * bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
   * </pre>
   *
   * @param vals map of name/nodes to replace
   * @param xml nodes containing lift:bind nodes
   *
   * @return the replaced xml nodes
   */
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = {
    xml.flatMap {
      node => node match {
        case s : Elem if (node.prefix == "lift" && node.label == "bind") => {
            node.attributes.get("name") match {
              case None => bind(vals, node.child)
              case Some(ns) => {
                  vals.get(ns.text) match {
                    case None => bind(vals, node.child)
                    case Some(nodes) => nodes
                  }
                }
            }
          }
        case Group(nodes) => Group(bind(vals, nodes))
        case s : Elem => Elem(node.prefix, node.label, node.attributes,node.scope, bind(vals, node.child) : _*)
        case n => node
      }
    }
  }

  /**
   * Bind a list of maps name/xml to a block of XML containing lift:bind nodes (see the bind(Map, NodeSeq) function)
   */
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Can[NodeSeq] = {
    def build (listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }

  /**
   * Bind parameters to XML.
   * @param around XML with lift:bind elements
   * @param atWhat data to bind
   * @deprecated use the bind function instead
   */
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]) : NodeSeq = {

    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
    s.projection.map(f).find(_.isDefined).getOrElse(None)

    around.flatMap {
      v =>
      v match {
        case Group(nodes) => Group(processBind(nodes, atWhat))
        case Elem("lift", "bind", attr @ _, _, kids @ _*) =>
          findMap(atWhat) {
            case (at, what) if attr("name").text == at => Some(what)
            case _ => None
          }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))

        case e: Elem => {Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)}
        case _ => {v}
      }

    }
  }
  /**
   * Looks for a named parameter in the XML element and return it if found
   *
   * @return a Full can containing the value of the found attribute if it is not empty
   */
  def xmlParam(in: NodeSeq, param: String): Can[String] = {
    val tmp = (in \ ("@" + param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }

  def findNode(in: Elem, nodes: NodeSeq): Can[Elem] = nodes match {
    case seq if seq.isEmpty => None
     case Seq(x: Elem, xs @_*)
       if x.label == in.label && x.prefix == in.prefix => Full(x)
     case Seq(x, xs @_*) => findNode(in, x.child) or findNode(in, xs)
  }
}

// vim: set ts=2 sw=2 et:
