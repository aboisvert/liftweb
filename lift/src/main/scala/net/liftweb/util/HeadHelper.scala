package net.liftweb.util
import scala.xml._

/**
 * This object provides functions to setup the head section of html documents.</p>
 * <code></code>
 */
object HeadHelper {

  def identity(xml: NodeSeq) : NodeSeq = xml

  def mergeToHtmlHead(xhtml: NodeSeq) : NodeSeq = {
    val headChildren = new NodeBuffer();
    def extractHead2(nodes: Iterable[Node]): NodeSeq = {
      val zs = new NodeBuffer();
      for(val z <- nodes) { zs &+ extractHead(z) }
      zs
    }
    def extractHead(node: NodeSeq):NodeSeq = node match {
      case Elem(namespace, "html", attrs, scp, children @ _*) => {
        val newChildren = extractHead2(children)
        val newHead = cleanHead(<head>{headChildren}</head>)
        Elem(namespace, "html", attrs, scp, (newHead ++ newChildren):_*)
      }
      case <head>{ ch @ _* }</head> => {
        ch foreach(c => {
          (headChildren find (sameAs(c, _))) match {
            case None => headChildren &+ c
            case _ =>
          }
        })
        Nil
      }
      case Elem(namespace, label, attrs, scp, ns @ _*) => Elem(namespace, label, attrs, scp, extractHead2(ns):_* )
      case Group(nodes) => nodes.flatMap(extractHead)
      case _ if (node.isInstanceOf[Node]) => node
      case _ => node.flatMap(extractHead2)
    }
    extractHead2(xhtml)
  }

  def cleanHead(htmlHead: NodeSeq) : NodeSeq = {
    val newHead = new NodeBuffer()
    htmlHead.foreach(node => if (!newHead.exists(sameAs(node, _))) newHead &+ node)
    newHead
  }

  def sameAs(node1: Node, node2: Node): Boolean = {
    if (node1.label.equals(node2.label)) {
      node1.label match {
        case "title" => true
        case label if (label == "style" || label == "script" || label == "link") => {
          (!node1.attribute("id").isEmpty && node1.attribute("id").equals(node2.attribute("id"))) ||
          (!node1.attribute("src").isEmpty && node1.attribute("src").equals(node2.attribute("src"))) ||
          (!node1.attribute("href").isEmpty && node1.attribute("href").equals(node2.attribute("href"))) ||
          (node1.child != NodeSeq.Empty && node1.child.equals(node2.child))
        }
        case _ => false
      }
    } else {
      false
    }
  }
}

