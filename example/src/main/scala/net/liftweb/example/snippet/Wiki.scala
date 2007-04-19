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
   * Get the XHTML containing a list of users
   */
  def main: NodeSeq = {
    val pageName = S.param("wiki_page") getOrElse "HomePage" // set the name of the page
    
    if (pageName == "all") { // if the page is "all" display all the pages
      for (val entry <- WikiEntry.findAll(OrderBy(WikiEntry.name, true))) 
        yield <div><a href={"/wiki/"+entry.name}>{entry.name}</a></div>      
    } else {
      // find the entry in the database or create a new one
      val entry = (for (val entry <- WikiEntry.find(By(WikiEntry.name, pageName)))
	yield entry) getOrElse 
      {
	val ret = new WikiEntry
	ret.name := pageName
	ret
      }
      
      // is it a new entry?
      val isNew = !entry.saved_?
      
      // show edit or just display
      val edit = isNew || (S.param("param1") getOrElse "no").toLowerCase == "edit"
      
      <span><a href="/wiki/all">Show All Pages</a><br/>{
	if (edit) {
	  <form method="POST" action={S.request.uri+"/"+pageName}>{ // the form tag
	    (if (isNew) Text("Create Entry named "+pageName) else Text("Edit entry named "+pageName)) ++ // the message
            <span>&nbsp;<a href="http://hobix.com/textile/quick.html" target="_blank">Textile Markup Reference</a><br /></span> ++ 
	    entry.entry.toForm ++ <br /> ++ // display the form
	    <a href={S.request.uri+"/"+pageName}>Cancel</a> ++ Text(" ") ++ S.submit(s => entry.save, Val(if (isNew) "Add" else "Edit")) // and links
	}</form>
    } else {
      // get the record and convert it to Textile format
      ((for (val p <- TextileParser.parse(entry.entry)) yield p._1.toHtml) getOrElse Text("")) ++
      <br/><a href={S.request.uri+"/"+pageName+"/edit"}>Edit</a> // and add an "edit" link
    }
      }</span>
    }
  }
}
