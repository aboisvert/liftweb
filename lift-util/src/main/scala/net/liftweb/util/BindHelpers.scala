package net.liftweb.util

/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
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

trait AttrHelper[+Holder[X]] {
  type Info

  def apply(key: String): Holder[Info] = convert(findAttr(key))
  def apply(prefix: String, key: String): Holder[Info] =
  convert(findAttr(prefix, key))

  def apply(key: String, default: => Info): Info =
  findAttr(key) getOrElse default

  def apply(prefix: String, key: String, default: => Info): Info =
  findAttr(prefix, key) getOrElse default

  def apply[T](key: String, f: Info => T): Holder[T] =
  convert(findAttr(key).map(f))

  def apply[T](prefix: String, key: String, f: Info => T): Holder[T] =
  convert(findAttr(prefix, key).map(f))
  def apply[T](key: String, f: Info => T, default: => T): T =
  findAttr(key).map(f) getOrElse default

  def apply[T](prefix: String, key: String, f: Info => T, default: => T): T =
  findAttr(prefix, key).map(f) getOrElse default

  protected def findAttr(key: String): Option[Info]
  protected def findAttr(prefix: String, key: String): Option[Info]
  protected def convert[T](in: Option[T]): Holder[T]
}

/**
 * BindHelpers can be used to have access to additional information while bind function is executing.
 * Such information refers to node attributes of the current bound node or the entire NodeSeq that is
 * to be bound. Since the context is created during bind execution and destroyed when bind terminates,
 * you can benefit of these helpers in the context of FuncBindParam or FuncAttrBindParam. You can of
 * course use your own implementation of BindParam and your BindParam#calcValue function will be called
 * in the appropriate context.
 *
 * <pre>
 * Example:
 *
 * bind("hello", xml,
 *   	"someNode" -> {node: NodeSeq => <function-body>})
 *
 * In <code>function-body</code> you can safely use the BindHelpers
 * </pre>
 *
 */
object BindHelpers extends BindHelpers {

  private val _bindNodes = new ThreadGlobal[List[NodeSeq]]
  private val _currentNode = new ThreadGlobal[Elem]

  /**
   * A list of NodeSeq that is behind bind.  The head of the list is the most
   * recent NodeSeq. Empty and Full(Nil) have different semantics here. It returns
   * empty if this function is called outside its context and Full(Nil) is returned if
   * there are no child nodes but the function is called from the appropriate context.
   */
  def bindNodes: Box[List[NodeSeq]] = _bindNodes.box

  /**
   * The current Elem, the children of which are passed to the bindParam
   */
  def currentNode: Box[Elem] = _currentNode.box

  /**
   * Helpers to look up attributes on the currentNode
   */
  object attr extends AttrHelper[Option] {
    type Info = NodeSeq

    protected def findAttr(key: String): Option[Info] =
    for {n  <- _currentNode.box.toOption
         at <- n.attributes.find(at => at.key == key && !at.isPrefixed)}
    yield at.value

    protected def findAttr(prefix: String, key: String): Option[Info] =
    for {n  <- _currentNode.box.toOption
         at <- n.attributes.find {
        case at: PrefixedAttribute => at.key == key && at.pre == prefix
        case _ => false
      }}
    yield at.value

    protected def convert[T](in: Option[T]): Option[T] = in

  }
}

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
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq =
    Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
    case Nil => NodeSeq.Empty
    case x :: xs => x.child
  }

  /**
   * Choose one of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag: String): Box[NodeSeq] =
  Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
    case Nil => Empty
    case x :: xs => Full(x.child)
  }

  /**
   * Choose two of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String): Box[(NodeSeq, NodeSeq)] =
  for (x1 <- template(xhtml, prefix, tag1);
       x2 <- template(xhtml, prefix, tag2)) yield (x1, x2)

  /**
   * Choose three of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String, tag3: String): Box[(NodeSeq, NodeSeq, NodeSeq)] =
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

  case class BoxBindParam(name: String, value: Box[NodeSeq]) extends Tuple2(name, value) with BindParam {
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
   * transforms a Box into a Text node
   */
  object BindParamAssoc {
    implicit def canStrBoxNodeSeq(in: Box[Any]): Box[NodeSeq] = in.map(_ match {
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
    def ->(in: NodeSeq) = TheBindParam(name, in)
    def ->(in: Text) = TheBindParam(name, in)
    def ->(in: Node) = TheBindParam(name, in)
    def ->(in: Seq[Node]) = TheBindParam(name, in)
    def ->(in: NodeSeq => NodeSeq) = FuncBindParam(name, in)
    def ->(in: Box[NodeSeq]) = BoxBindParam(name, in)
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
    def -->(value: Box[NodeSeq]): BindParam = TheBindParam(name, value.openOr(Text("Empty")))
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
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           xml: NodeSeq, params: BindParam*): NodeSeq = {
    BindHelpers._bindNodes.doWith(xml :: (BindHelpers._bindNodes.box.openOr(Nil))) {
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
          case s : Elem if s.prefix == namespace => BindHelpers._currentNode.doWith(s) {
              map.get(s.label) match {
                case None =>
                  nodeFailureXform.map(_(s)) openOr s

                case Some(ns) =>
                  val toRet = ns.calcValue(s.child)
                  mergeBindAttrs(toRet, namespace, s.attributes)
              }
            }
          case Group(nodes) => Group(in_bind(nodes))
          case s : Elem => Elem(s.prefix, s.label, attrBind(s.attributes), s.scope, in_bind(s.child) : _*)
          case n => n
        }
      }


      in_bind(xml)
    }
  }

  private def setElemId(in: NodeSeq, attr: String, value: Seq[Node]): NodeSeq =
  in.map {
    case e: Elem => e % new UnprefixedAttribute(attr, value, Null)
    case v => v
  }

  private def mergeBindAttrs(in: NodeSeq, nameSpace: String, attrs: MetaData): NodeSeq = attrs match {
    case Null => in
    case p: PrefixedAttribute if p.pre == nameSpace =>
      mergeBindAttrs(setElemId(in, p.key, p.value), nameSpace, p.next)
    case m => mergeBindAttrs(in, nameSpace, m.next)
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
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Box[NodeSeq] = {
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
  def xmlParam(in: NodeSeq, param: String): Box[String] = {
    val tmp = (in \ ("@" + param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }

  def findNode(in: Elem, nodes: NodeSeq): Box[Elem] = nodes match {
    case seq if seq.isEmpty => None
    case Seq(x: Elem, xs @_*)
      if x.label == in.label && x.prefix == in.prefix => Full(x)
    case Seq(x, xs @_*) => findNode(in, x.child) or findNode(in, xs)
  }
}

// vim: set ts=2 sw=2 et:
