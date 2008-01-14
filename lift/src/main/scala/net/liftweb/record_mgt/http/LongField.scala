package net.liftweb.record_mgt.http;

import net.liftweb._
import http._
import record._
import scala.xml._

trait TextForm { self: SimpleField =>
  def toForm: NodeSeq = S.text(toString, v => fromString(v).foreach(set(_)))
}

class LongField[OwnerType <: Record[OwnerType]](owner: OwnerType) extends LongFieldProto[OwnerType](owner) with JdbcLocator 
with XmlLocator
with TextForm {
  
}
