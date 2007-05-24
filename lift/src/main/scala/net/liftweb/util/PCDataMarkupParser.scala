package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.xml.parsing.{MarkupParser, MarkupHandler, FatalError, ConstructingHandler, ExternalSources}
import scala.xml.{Unparsed, NodeSeq, Atom, Elem, Comment, Group, NamespaceBinding, Node, SpecialNode, Text, TopScope}
import scala.io.{Source}
import java.io.{InputStream}

/**
 * Extends the Markup Parser to do the right thing (tm) with PCData blocks
 */
trait PCDataMarkupParser requires (MarkupParser with MarkupHandler) extends MarkupParser {
  /** '&lt;! CharData ::= [CDATA[ ( {char} - {char}"]]&gt;"{char} ) ']]&gt;'
   *
   * see [15]
   */
  override def xCharData: NodeSeq = {
    xToken("[CDATA[")
    val pos1 = pos
    val sb: StringBuilder = new StringBuilder()
    while (true) {
      if (ch==']'  &&
          { sb.append(ch); nextch; ch == ']' } &&
          { sb.append(ch); nextch; ch == '>' } ) {
            sb.setLength(sb.length() - 2);
            nextch; 
            return PCData(sb.toString)
          } else sb.append( ch );
      nextch; 
    }
    throw FatalError("this cannot happen");
  }
}

class PCDataXmlParser(val input: Source) extends ConstructingHandler with PCDataMarkupParser with ExternalSources  {
  val preserveWS = true
  // val input = from
}

object PCDataXmlParser {
  def apply(in: InputStream): Option[NodeSeq] = {
    val source = Source.fromInputStream(in)
    val p = new PCDataXmlParser(source)
    while (p.ch != '<' && p.curInput.hasNext) p.nextch
    p.document match {
      case null => None
      case doc => Some(doc)
    }
  }

  def apply(in: String): Option[NodeSeq] = {
    val source = Source.fromString(in)
    val p = new PCDataXmlParser(source)
    while (p.ch != '<' && p.curInput.hasNext) p.nextch
    p.document match {
      case null => None
      case doc => Some(doc)
    }
  }
}

case class PCData(_data: String) extends Atom[String](_data) {
  /* The following code is a derivative work of scala.xml.Text */
  if (null == data)
    throw new java.lang.NullPointerException("tried to construct Text with null")

  final override def equals(x: Any) = x match {
    case s:String  => s.equals(data.toString())
    case s:Atom[Any]    => data == s.data
    case _ => false
  }

  /** Returns text, with some characters escaped according to the XML
   *  specification.
   *
   *  @param  sb ...
   *  @return ... 
   */
  override def toString(sb: StringBuilder) = {
    sb.append("<![CDATA[")
    sb.append(data)
    sb.append("]]>")
  }
}

object AltXML {
  def toXML(n: Node, stripComment: Boolean): String = {
    val sb = new StringBuilder()
    toXML(n, TopScope, sb, stripComment)
    sb.toString()
  }


  /** 
   * Appends a tree to the given stringbuffer within given namespace scope.
   *
   * @param n            the node
   * @param pscope       the parent scope
   * @param sb           stringbuffer to append to
   * @param stripComment if true, strip comments
   */
  def toXML(x: Node, pscope: NamespaceBinding, sb: StringBuilder, stripComment: Boolean): Unit = {
    x match {

      case c: Comment if !stripComment =>
        c.toString(sb)

      case x: SpecialNode =>
        x.toString(sb)

      case g: Group =>
        for (c <- g.nodes) toXML(c, x.scope, sb, stripComment)

      case _  =>
        if (((x.child eq null) || (x.child.length == 0)) && x.label != "script") {
          sb.append('<')
          x.nameToString(sb)
          if (x.attributes ne null) x.attributes.toString(sb)
          x.scope.toString(sb, pscope)
          sb.append(" />")
        } else {
        // print tag with namespace declarations
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.toString(sb)
        x.scope.toString(sb, pscope)
        sb.append('>')
        sequenceToXML(x.child, pscope, sb, stripComment)
        sb.append("</")
        x.nameToString(sb)
        sb.append('>')
        }
    }
  }

  /**
   * @param children     ...
   * @param pscope       ...
   * @param sb           ...
   * @param stripComment ...
   */
  def sequenceToXML(children: Seq[Node], pscope: NamespaceBinding,
                    sb: StringBuilder, stripComment: Boolean): Unit = {
    if (children.isEmpty)
      return
    else if (children forall { y => y.isInstanceOf[Atom[Any]] && !y.isInstanceOf[Text] }) { // add space
      val it = children.elements
      val f = it.next
      toXML(f, f.scope, sb, stripComment)
      while (it.hasNext) {
        val x = it.next
        sb.append(' ')
        toXML(x, x.scope, sb, stripComment)
      }
    } else {
      for (c <- children) toXML(c, c.scope, sb, stripComment)
    }
  }
  
}
