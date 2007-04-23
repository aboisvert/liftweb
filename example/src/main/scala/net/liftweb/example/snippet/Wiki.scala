package net.liftweb.example.snippet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.textile._

class Wiki {
  /**
    * Display the Textile marked up wiki or an edit box
    */
  def main: NodeSeq = {
    val pageName = S.param("wiki_page") getOrElse "HomePage" // set the name of the page
    
    def showAll = {
      WikiEntry.findAll(OrderBy(WikiEntry.name, true)).flatMap(entry =>
      <div><a href={"/wiki/"+entry.name}>{entry.name}</a></div>)
    }
    
    def editEntry(entry: WikiEntry, isNew: boolean) = {
      val action = S.request.uri+"/"+pageName
      
      val message = if (isNew) Text("Create Entry named "+pageName) else Text("Edit entry named "+pageName)
      
      val hobixLink = <span>&nbsp;<a href="http://hobix.com/textile/quick.html" target="_blank">Textile Markup Reference</a><br /></span>
      
      val cancelLink = <a href={S.request.uri+"/"+pageName}>Cancel</a>
      
      val submitButton = S.submit(s => entry.save, Val(if (isNew) "Add" else "Edit"))
      
      <form method="POST" action={action}>{ // the form tag
            message ++ 
            hobixLink ++ 
            entry.entry.toForm ++ // display the form  
            <br /> ++ 
            cancelLink ++ 
            Text(" ") ++ 
            submitButton  
        }</form>
    }
    
    if (pageName == "all") showAll // if the page is "all" display all the pages
    else {
      // find the entry in the database or create a new one
      val entry = WikiEntry.find(By(WikiEntry.name, pageName)) getOrElse WikiEntry.create.name(pageName)
      
      // is it a new entry?
      val isNew = !entry.saved_?
      
      // show edit or just display
      val edit = isNew || (S.param("param1").map(_ == "edit") getOrElse false)
      
      <span><a href="/wiki/all">Show All Pages</a><br/>{
	if (edit) editEntry(entry, isNew)
        else TextileParser.toHtml(entry.entry) ++ 
             <br/><a href={S.request.uri+"/"+pageName+"/edit"}>Edit</a> // and add an "edit" link
      }</span>
    }
  }
}
