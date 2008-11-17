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
package net.liftweb.http;

import S._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.AjaxInfo
import JE._
import JsCmds._
import _root_.scala.xml._

object SHtml {
  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: NodeSeq, func: () => JsCmd): Elem =
    <button onclick={makeAjaxCall(Str(mapFunc(func)+"=true"))}>{text}</button> // <input type="button" value={text}/> % ("onclick" -> makeAjaxCall(Str(mapFunc(func)+"=true")))

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String, func: () => JsCmd): Elem =
  ajaxButton(Text(text), func)
//    <button onclick={makeAjaxCall(Str(mapFunc(func)+"=true"))}>{text}</button>

  /**
   * create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
  def a(func: () => JsCmd, body: NodeSeq): Elem = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(key, (a: List[String]) => func())
      (<lift:a key={key}>{body}</lift:a>)
  }

  def makeAjaxCall(in: JsExp): JsExp = new JsExp {
    def toJsCmd = "lift_ajaxHandler("+ in.toJsCmd+", null, null)"
  }

  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   */
  def a(body: NodeSeq)(func: => JsCmd): Elem = a(() => func, body)

  /**
   * Create an anchor that will run a JavaScript command when clicked
   */
  def a(body: NodeSeq, cmd: JsCmd): Elem = (<a href="javascript://" onclick={cmd.toJsCmd + "; return false;"}>{body}</a>)

  /**
   * Create a span that will run a JavaScript command when clicked
   */
  def span(body: NodeSeq, cmd: JsCmd): Elem = (<span onclick={cmd.toJsCmd}>{body}</span>)

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  def ajaxCall(jsCalcValue: JsExp, func: String => JsCmd): JsExp = ajaxCall_*(jsCalcValue, SFuncHolder(func))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: JsExp, func: AFuncHolder): JsExp =
  makeAjaxCall(JsRaw("'"+mapFunc(func)+"=' + "+jsCalcValue.toJsCmd))


  def toggleKids(head: Elem, visible: Boolean, func: () => Any, kids: Elem): NodeSeq = {
    val funcName = mapFunc(func)
    val (nk, id) = findOrAddId(kids)
    val rnk = if (visible) nk else nk % ("style" -> "display: none")
    val nh = head % ("onclick" -> (LiftRules.jsArtifacts.toggle(id).cmd & makeAjaxCall(JsRaw("'"+funcName+"=true'")).cmd))
    nh ++ rnk
  }

  /**
   * Create a JSON text widget that makes a JSON call on blur or "return".
   * Note that this is not "Stateful" and will be moved out of S at some
   * point.
   *
   * @param value - the initial value of the text field
   * @param json - takes a JsExp which describes how to recover the
   * value of the text field and returns a JsExp containing the thing
   * to execute on blur/return
   *
   * @return a text field
   */
  def jsonText(value: String, json: JsExp => JsCmd): Elem = {
    (<input type="text" value={value}/>) %
    ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
    ("onblur" -> (json(JE.JsRaw("this.value"))))
  }

  def ajaxText(value: String, func: String => JsCmd): Elem = ajaxText_*(value, SFuncHolder(func))

  private def ajaxText_*(value: String, func: AFuncHolder): Elem = {
    val funcName = mapFunc(func)
      (<input type="text" value={value}/>) %
        ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
        ("onblur" -> makeAjaxCall(JsRaw("'" +funcName + "=' + encodeURIComponent(this.value)")))
  }

  def ajaxCheckbox(value: Boolean, func: Boolean => JsCmd): Elem = ajaxCheckbox_*(value, LFuncHolder(in =>  func(in.exists(toBoolean(_)))))

  private def ajaxCheckbox_*(value: Boolean, func: AFuncHolder): Elem = {
    val funcName = mapFunc(func)
      (<input type="checkbox"/>) % checked(value) % ("onclick" -> makeAjaxCall(JsRaw("'" + funcName+"='+this.checked")))
  }

  def ajaxSelect(opts: Seq[(String, String)], deflt: Can[String], func: String => JsCmd): Elem = ajaxSelect_*(opts, deflt, SFuncHolder(func))

  private def ajaxSelect_*(opts: Seq[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    val funcName = mapFunc(testFunc)

    (<select>{
       opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>) % ("onchange" -> makeAjaxCall(JsRaw("'" + funcName+"='+this.options[this.selectedIndex].value")))
  }

  def ajaxInvoke(func: () => JsCmd): JsExp = makeAjaxCall(Str(mapFunc(NFuncHolder(func)) + "=true"))

  /**
   * Build a swappable visual element.  If the shown element is clicked on, it turns into the hidden element and when
   * the hidden element blurs, it swaps into the shown element.
   */
  def swappable(shown: Elem, hidden: Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val (rh, hid) = findOrAddId(hidden)
    val ui = LiftRules.jsArtifacts
    (<span>{rs % ("onclick" -> (ui.hide(sid).cmd &
                                 ui.showAndFocus(hid).cmd & JsRaw("return false;")))}
           {dealWithBlur(rh % ("style" -> "display: none"), (ui.show(sid).cmd & ui.hide(hid).cmd))}
     </span>)
  }

  def swappable(shown: Elem, hidden: String => Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val hid = "S"+randomString(10)
    val ui = LiftRules.jsArtifacts

    val rh = <span id={hid}>{hidden(ui.show(sid).toJsCmd + ";" + ui.hide(hid).toJsCmd + ";")}</span>
      (<span>{rs % ("onclick" -> (ui.hide(sid).toJsCmd + ";" + ui.show(hid).toJsCmd + "; return false;"))}{
         (rh % ("style" -> "display: none"))}</span>)
  }

  private def dealWithBlur(elem: Elem, blurCmd: String): Elem = {
   (elem \ "@onblur").toList match {
      case Nil => elem % ("onblur" -> blurCmd)
      case x :: xs => val attrs = elem.attributes.filter(_.key != "onblur")
         Elem(elem.prefix, elem.label, new UnprefixedAttribute("onblur", Text(blurCmd + x.text), attrs), elem.scope, elem.child :_*)
     }
   }


  /**
   * create an anchor tag around a body
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
  def link(to: String, func: () => Any, body: NodeSeq): Elem = {
    val key = mapFunc((a: List[String]) => {func(); true})
      (<a href={to+"?"+key+"=_"}>{body}</a>)
  }

  private def makeFormElement(name: String, func: AFuncHolder): Elem = (<input type={name} name={mapFunc(func)}/>)

  def text_*(value: String, func: AFuncHolder): Elem = makeFormElement("text", func) % new UnprefixedAttribute("value", Text(value), Null)
  def password_*(value: String, func: AFuncHolder): Elem = makeFormElement("password", func) % new UnprefixedAttribute("value", Text(value), Null)
  def hidden_*(func: AFuncHolder): Elem = makeFormElement("hidden", func) % ("value" -> "true")
  def submit_*(value: String, func: AFuncHolder): Elem = makeFormElement("submit", func) % new UnprefixedAttribute("value", Text(value), Null)
  def text(value: String, func: String => Any): Elem = makeFormElement("text", SFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)
  def password(value: String, func: String => Any): Elem = makeFormElement("password", SFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)
  def hidden(func: () => Any): Elem = makeFormElement("hidden", NFuncHolder(func)) % ("value" -> "true")
  def submit(value: String, func: () => Any): Elem = makeFormElement("submit", NFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)

  def ajaxForm(body: NodeSeq) = (<lift:form>{body}</lift:form>)
  def ajaxForm(onSubmit: JsCmd, body: NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)
  def ajaxForm(body: NodeSeq, onSubmit: JsCmd) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)

  def jsonForm(jsonHandler: JsonHandler, body: NodeSeq): NodeSeq = jsonForm(jsonHandler, Noop, body)
  def jsonForm(jsonHandler: JsonHandler, onSubmit: JsCmd, body: NodeSeq): NodeSeq = {
    val id = "F"+randomString(15)
    <form onsubmit={(onSubmit & jsonHandler.call("processForm", FormToJSON(id)) & JsReturn(false)).toJsCmd} id={id}>
      {body}
    </form>
  }

  private[http] def secureOptions[T](options: Seq[(T, String)], default: Can[T],
                    onSubmit: T => Unit) = {
     val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
     val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
     val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
     def process(nonce: String): Unit =
       secure.find(_._2 == nonce).map(x => onSubmit(x._1))
    (nonces, defaultNonce, SFuncHolder(process))
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs (value, text to display)
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
   def select(opts: Seq[(String, String)], deflt: Can[String], func: String => Any): Elem =
     select_*(opts, deflt, SFuncHolder(func))

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options  -- a list of value and text pairs (value, text to display)
   * @param default  -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
   def selectObj[T](options: Seq[(T, String)], default: Can[T],
                    onSubmit: T => Unit): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) =
      secureOptions(options, default, onSubmit)

    select_*(nonces, defaultNonce, secureOnSubmit)
   }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select_*(opts: Seq[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)

    (<select name={mapFunc(testFunc)}>{
      opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>)
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect(opts: Seq[(String, String)], deflt: Can[String], func: String => Any): Elem = untrustedSelect_*(opts, deflt, SFuncHolder(func))

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect_*(opts: Seq[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    (<select name={mapFunc(func)}>{
      opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>)
  }


  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "selected", Null) else Null

  def multiSelect(opts: Seq[(String, String)], deflt: Seq[String], func: String => Any): Elem = multiSelect_*(opts, deflt, SFuncHolder(func))

  def multiSelect_*(opts: Seq[(String, String)], deflt: Seq[String],func: AFuncHolder): Elem = (<select multiple="true" name={mapFunc(func)}>{
    opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))
  }</select>)


  def textarea(value: String, func: String => Any): Elem = textarea_*(value, SFuncHolder(func))

  def textarea_*(value: String, func: AFuncHolder): Elem = (<textarea name={mapFunc(func)}>{value}</textarea>)

  def radio(opts: Seq[String], deflt: Can[String], func: String => Any): ChoiceHolder[String] =
    radio_*(opts, deflt, SFuncHolder(func))

  def radio_*(opts: Seq[String], deflt: Can[String], func: AFuncHolder): ChoiceHolder[String] = {
    val name = mapFunc(func)
    val itemList = opts.map(v => ChoiceItem(v, (<input type="radio" name={name} value={v}/>) %
      checked(deflt.filter((s: String) => s == v).isDefined)))
      ChoiceHolder(itemList)
  }

  def fileUpload(func: FileParamHolder => Any): Elem = <input type="file" name={mapFunc(BinFuncHolder(func))} />

  case class ChoiceItem[T](key: T, xhtml: NodeSeq)

  case class ChoiceHolder[T](items: Seq[ChoiceItem[T]]) {
    def apply(in: T) = items.filter(_.key == in).first.xhtml
    def apply(in: Int) = items(in).xhtml
    def map[A](f: ChoiceItem[T] => A) = items.map(f)
    def flatMap[A](f: ChoiceItem[T] => Iterable[A]) = items.flatMap(f)
    def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)
    def toForm: NodeSeq = flatMap(c => (<span>{c.xhtml}&nbsp;{c.key.toString}<br /></span>))
  }

  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null
  private def setId(in: Can[String]) = in match { case Full(id) => new UnprefixedAttribute("id", Text(id), Null); case _ => Null}

  def checkbox[T](possible: Seq[T], actual: Seq[T], func: Seq[T] => Any): ChoiceHolder[T] = {
    val len = possible.length
    val name = mapFunc(LFuncHolder( (strl: List[String]) => {func(strl.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true}))

    ChoiceHolder(possible.toList.zipWithIndex.map(p =>
    ChoiceItem(p._1, (<input type="checkbox" name={name} value={p._2.toString}/>) % checked(actual.contains(p._1)) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
  }

  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the
   * checkbox is submitted.
   */
  def checkbox(value: Boolean, func: Boolean => Any): NodeSeq = {
    checkbox_id(value, func, Empty)
  }

  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the
   * checkbox is submitted. Has an id of {@code id}.
   */
  def checkbox_id(value: Boolean, func: Boolean => Any, id: Can[String]): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
        true
    }
    checkbox_*(value, LFuncHolder(from(func)), id)
  }

  def checkbox_*(value: Boolean, func: AFuncHolder, id: Can[String]): NodeSeq = {
    val name = mapFunc(func)
    (<input type="hidden" name={name} value="false"/>) ++
      ((<input type="checkbox" name={name} value="true" />) % checked(value) % setId(id))
  }

}
