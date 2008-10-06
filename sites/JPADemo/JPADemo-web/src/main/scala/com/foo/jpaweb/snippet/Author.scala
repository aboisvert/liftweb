package com.foo.jpaweb.snippet

import scala.xml.{NodeSeq,Text}

import net.liftweb.http.{RequestVar,S,SHtml}
import net.liftweb.util.Helpers
import S._
import Helpers._

import com.foo.jpaweb.model._
import Model._

class AuthorOps {
  def list (xhtml : NodeSeq) : NodeSeq = {
    val authors = Model.createNamedQuery[Author]("findAllAuthors").getResultList()

    authors.flatMap(author =>
      bind("author", xhtml,
	   "name" -> Text(author.name),
	   "count" -> SHtml.link("/books/search.html", {() =>
	     BookOps.resultVar(Model.createNamedQuery[Book]("findBooksByAuthor", "id" ->author.id).getResultList().toList)
	     }, Text(author.books.size().toString)),
	   "edit" -> SHtml.link("add.html", () => authorVar(author), Text(?("Edit")))))
  }

  // Set up a requestVar to track the author object for edits and adds
  object authorVar extends RequestVar(new Author())
  def author = authorVar.is

  def add (xhtml : NodeSeq) : NodeSeq = {
    def doAdd () = {
      if (author.name.length == 0) {
	error("emptyAuthor", "The author's name cannot be blank")
      } else {
	Model.merge(author)
	redirectTo("list.html")
      }
    }

    // Hold a val here so that the "id" closure holds it when we re-enter this method
    val currentId = author.id

    bind("author", xhtml,
	 "id" -> SHtml.hidden({author.id = currentId}),
	 "name" -> SHtml.text(author.name, author.name = _),
	 "submit" -> SHtml.submit(?("Save"), doAdd))
  }
}
