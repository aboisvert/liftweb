/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.builtin.snippet;

import net.liftweb.http._
import S._
import scala.xml._
import net.liftweb.util.Helpers._

/**
 * This class is a built in snippet that renders the messages (Errors, Warnings, Notices). Typically it is used in templates
 * as a place holder for any messages set by user that are not associated with an ID.
 *
 * E.g. (child nodes are optional)
 * <pre>
 * <lift:snippet type="error_report">
 *   <lift:error_msg>Error!  The details are:</lift:error_msg>
 *   <lift:error_class>errorBox</lift:error_class>
 *   <lift:warning_msg>Whoops, I had a problem:</lift:warning_msg>
 *   <lift:warning_class>warningBox</lift:warning_class>
 *   <lift:notice_msg>Note:</lift:notice_msg>
 *   <lift:notice_class>noticeBox</lift:notice_class>
 * </lift:snippet>
 * </pre>
 *
 */
class Msgs {
  def render(styles: NodeSeq): NodeSeq = {
    val f = noIdMessages _
    val msgs = List((f(S.errors), (styles \\ "error_msg"), S.??("msg.error"), ((styles \\ "error_class") ++ (styles \\ "error_msg" \\ "@class"))),
        (f(S.warnings), (styles \\ "warning_msg"), S.??("msg.warning"), ((styles \\ "warning_class")++ (styles \\ "error_msg" \\ "@class"))),
        (f(S.notices), (styles \\ "notice_msg"), S.??("msg.notice"), ((styles \\ "notice_class")) ++ (styles \\ "notice_msg" \\ "@class"))).flatMap {
        case (msg, titleList, defaultTitle, styleList) =>
          val title: String = titleList.toList.filter(_.prefix == "lift").map(_.text.trim).filter(_.length > 0) headOr defaultTitle

          msg.toList.map(e => (<li>{e}</li>) ) match {
            case Nil => Nil
            case msgList => val ret = (<div>{title}<ul>{msgList}</ul></div>)
            styleList.toList.map(_.text.trim).foldLeft(ret)((xml, style) => xml % new UnprefixedAttribute("class", Text(style), Null))
          }
       }
    <div>{msgs}</div> % ("id" -> LiftRules.noticesContainerId)
  }
}
