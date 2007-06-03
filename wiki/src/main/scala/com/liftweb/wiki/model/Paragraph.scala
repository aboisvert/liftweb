package com.liftweb.wiki.model

import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.mapper._
import net.liftweb.textile._
import net.liftweb.util.Helpers._
import java.util.regex._

class Paragraph extends KeyedMapper[long, Paragraph] {
  object id extends MappedLongIndex(this)
  object title extends MappedString(this, 100)
  object position extends MappedInt(this)
  //object page extends MappedLongForeignKey(this, Page)
  object pageKey extends MappedLong(this)
  //object revision extends MappedLongForeignKey(this, Revision)
  object revisionKey extends MappedLong(this)

  def getSingleton = Paragraph
  def primaryKeyField = id

  def page =
    Page.find(pageKey)
  
  def revision =
    Revision.find(revisionKey)

  def addRevision(next: Revision) {
    //transaction {
      next.previousKey := revisionKey
      next.save
      this.revisionKey := next.id
      this.save
      notice("new revision id: " + next.id.get)
      notice("revision id of paragraph: " + revisionKey.get)
    //}
  }

  def rollback {
    revisionKey := revision.previous.id
    save
  }

  def revisions {
    def inner(current: Revision): List[Revision] = 
      current.previous match {
        case Some(next) => current :: inner(next)
        case None => Nil
      }
    inner(revision)
  }

  def text: String =
    revision match {
      case Some(revision) =>
        revision.text
      case None =>
        "no revision of this paragraph available"
    }

  def html =
    <div>
      <h3>{ title }</h3>
      { TextileParser.toHtml(text) }
    </div>

}

object Paragraph extends Paragraph with KeyedMetaMapper[long, Paragraph]
