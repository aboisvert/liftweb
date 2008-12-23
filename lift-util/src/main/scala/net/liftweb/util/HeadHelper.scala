package net.liftweb.util

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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


import _root_.scala.xml._
import _root_.scala.xml.transform._

/**
 * This object provides functions to setup the head section of html documents.</p>
 * <code></code>
 */
object HeadHelper {
  def identity(xml: NodeSeq) : NodeSeq = xml

  def mergeToHtmlHead(xhtml: NodeSeq) : NodeSeq = {
    def trimText(in: NodeSeq): NodeSeq = in flatMap {
      case e: Elem =>
        Elem(e.prefix, e.label, e.attributes, e.scope, trimText(e.child) :_*)

      case g: Group =>
        trimText(g.child)

      case t: Text =>
        val s = t.text.trim
        if (s.length == 0) NodeSeq.Empty
        Text(s)

      case x => x
    }

    val headInBody: NodeSeq =
    (for (body <- xhtml \ "body";
          head <- body \\ "head") yield trimText(head.child)).
    toList.removeDuplicates.flatMap(a => a)

    if (headInBody.isEmpty) xhtml
    else {
      def xform(in: NodeSeq, inBody: Boolean): NodeSeq = in flatMap {
        case e: Elem if !inBody && e.label == "body" =>
          Elem(e.prefix, e.label, e.attributes, e.scope, xform(e.child, true) :_*)

        case e: Elem if inBody && e.label == "head" => NodeSeq.Empty

        case e: Elem if e.label == "head" =>
          Elem(e.prefix, e.label, e.attributes,
               e.scope, e.child ++ headInBody :_*)

        case e: Elem =>
          Elem(e.prefix, e.label, e.attributes, e.scope, xform(e.child, inBody) :_*)

        case g: Group =>
          xform(g.child, inBody)

        case x => x
      }

      xform(xhtml, false)
    }
  }


}

