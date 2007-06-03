package com.liftweb.wiki.model

import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import java.util.regex._

class Page extends KeyedMapper[long, Page] {
  object id extends MappedLongIndex(this)
  object title extends MappedString(this, 100)

  def getSingleton = Page
  def primaryKeyField = id

  def paragraphs =
    Paragraph.findAll(By(Paragraph.pageKey, id.get))
      .sort(_.position < _.position)

  def appendParagraph(paragraph: Paragraph) {
    paragraph.position := paragraphs.length
    paragraph.pageKey := id
    paragraph.save
  }

  def insertParagraph(paragraph: Paragraph) {
    paragraphs.filter(_.position >= paragraph.position)
      .map(paragraph => {
          paragraph.position := paragraph.position.get + 1
          paragraph.save
      })
    paragraph.pageKey := id
    paragraph.save
  }

  def html: scala.xml.NodeSeq =
    <h2>{ title.toString }</h2> ++
      paragraphs.foldLeft[scala.xml.NodeSeq](<div></div>)((acc, p) => acc ++ p.html)
}

object Page extends Page with KeyedMetaMapper[long, Page]
