/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.example.lib

import _root_.net.liftweb._
import textile._
import util._
import Helpers._
import http._
import mapper._
import sitemap._
import Loc._

import example._
import model._

import scala.xml.{Text, NodeSeq}

case class WikiLoc(page: String, edit: Boolean) extends LocParams {
  lazy val record: WikiEntry =
    WikiEntry.find(By(WikiEntry.name, page)) openOr
  WikiEntry.create.name(page)
}

object WikiStuff extends Loc[WikiLoc] {
  object AllLoc extends WikiLoc("all", false)

  def name = "wiki"
  def defaultParams: Can[WikiLoc] = Full(WikiLoc("HomePage", false))

  def stuff: List[LocStuff] = Nil

  def currentEdit = foundParam.is.map(_.edit) openOr false

  override val snippets: SnippetTest = {
    case ("wiki", Full(AllLoc)) => showAll _
    case ("wiki", Full(wp @ WikiLoc(_ , true))) => editRecord(wp.record) _
    case ("wiki", Full(wp @ WikiLoc(_ , false)))
    if !wp.record.saved_? => editRecord(wp.record) _
    
    case ("wiki", Full(wp: WikiLoc)) => displayRecord(wp.record) _
  }


  def showAll(in: NodeSeq): NodeSeq = 
    WikiEntry.findAll(OrderBy(WikiEntry.name, Ascending)).flatMap(entry =>
      <div><a href={url(entry.name)}>{entry.name}</a></div>)
  
  def url(page: String) = createLink(WikiLoc(page, false))


  def editRecord(r: WikiEntry)(in: NodeSeq): NodeSeq = 
    <span>
  <a href={createLink(AllLoc)}>Show All Pages</a><br />
  {
    val isNew = !r.saved_?
    val pageName = r.name.is
    val action = url(pageName)
    val message = 
      if (isNew) 
	Text("Create Entry named "+pageName) 
      else 
	Text("Edit entry named "+pageName)
    
    val hobixLink = <span>&nbsp;<a href="http://hobix.com/textile/quick.html" target="_blank">Textile Markup Reference</a><br /></span>

    val cancelLink = <a href={action}>Cancel</a>
    val textarea = r.entry.toForm
    
    val submitButton = SHtml.submit(isNew ? "Add" | "Edit", r.save)
    
    <form method="POST" action={action}>{ // the form tag
      message ++
      hobixLink ++
      textarea ++ // display the form
      <br /> ++
      cancelLink ++
      Text(" ") ++
      submitButton
    }</form>
  }

  </span>

  def displayRecord(entry: WikiEntry)(in: NodeSeq): NodeSeq = 
    <span>
  <a href={createLink(AllLoc)}>Show All Pages</a><br />
  {TextileParser.toHtml(entry.entry, textileWriter)}

  <br/><a href={createLink(WikiLoc(entry.name, true))}>Edit</a>
  </span>

  val textileWriter = Some((info: TextileParser.WikiURLInfo) =>
    {
      import TextileParser._
      info match {
	case WikiURLInfo(page, _) => 
	  (stringUrl(page), Text(page), None)
      }
    }
			 )

  def stringUrl(page: String): String = 
    url(page).map(_.text) getOrElse ""

  val link = 
    new Loc.Link[WikiLoc](List("wiki"), false) {
      override def createLink(in: WikiLoc): Can[NodeSeq] = {
	if (in.edit)
	  Full(Text("/wiki/edit/"+urlEncode(in.page)))
	else
	  Full(Text("/wiki/"+urlEncode(in.page)))
      }
    }

  val text = 
    new Loc.LinkText[WikiLoc](calcLinkText _)
  
  
  def calcLinkText(in: WikiLoc): NodeSeq =
    if (in.edit)
      Text("Wiki edit "+in.page)
    else
      Text("Wiki "+in.page)

  override val rewrite: Can[PartialFunction[RewriteRequest, (RewriteResponse, WikiLoc)]] = 
    Full({
      case RewriteRequest(ParsePath("wiki" :: "edit" :: page :: Nil, _, _,_),
			  _, _) =>
			    println("Got wiki edit")
      (RewriteResponse("wiki" :: Nil), WikiLoc(page, true))

      case RewriteRequest(ParsePath("wiki" :: page :: Nil, _, _,_),
			  _, _) =>
			    println("Got wiki with page "+page)
      (RewriteResponse("wiki" :: Nil), WikiLoc(page, false))

    })
}

