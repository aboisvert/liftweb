package com.liftweb.wiki.actor

import scala.xml._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.http.{Page => HTTPPage, _}
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.mapper._
import com.liftweb.wiki.model._
import com.liftweb.wiki.controller._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object LockManager {
  val instance = {
    val instance = new LockManager
    instance.start
    instance
  }
}

class LockManager extends Actor {
  private val observers = new HashMap[Paragraph, ListBuffer[Editor]]
  private val locks = new HashMap[Paragraph, Editor]

  def act = {
    loop {
      react {
        case Register(paragraph, editor) =>
          register(paragraph, editor)
        case Unregister(paragraph, editor) =>
          unregister(paragraph, editor)
        case Lock(paragraph, editor) =>
          Console.println(paragraph + " locked by " + editor)
          reply(lock(paragraph, editor))
        case Unlock(paragraph, editor) =>
          Console.println(paragraph + " unlocked by " + editor)
          reply(unlock(paragraph, editor))
        case HasLock(paragraph, editor) =>
          Console.println("has " + editor + " the lock of " + paragraph)
          reply(hasLock(paragraph, editor))
        case IsLocked(paragraph) => 
          Console.println(paragraph + " is locked")
          reply(isLocked(paragraph))
      }
    }
  }

  private def register(paragraph: Paragraph, editor: Editor) {
    if (!observers.contains(paragraph))
      observers += paragraph -> new ListBuffer[Editor]
    observers(paragraph) += editor
  }

  private def unregister(paragraph: Paragraph, editor: Editor) {
    if (observers contains paragraph)
      observers(paragraph) -= editor
  }

  private def lock(paragraph: Paragraph, editor: Editor) =
    locks.get(paragraph) match {
      case None =>
        locks += paragraph -> editor
        observers(paragraph) foreach (_ ! Locked)
        true
      case _ =>
        false
    }

  private def unlock(paragraph: Paragraph, editor: Editor) = 
    locks.get(paragraph) match {
      case Some(owner) if owner == editor =>
        locks -= paragraph
        observers(paragraph) foreach (_ ! Unlocked)
        true
      case _ =>
        false
    }

  private def hasLock(paragraph: Paragraph, editor: Editor) =
    locks.get(paragraph) match {
      case Some(owner) if owner == editor =>
        true
      case _ =>
        false
    }

  private def isLocked(paragraph: Paragraph) =
    locks contains paragraph
}

case class Register(paragraph: Paragraph, editor: Editor)
case class Unregister(paragraph: Paragraph, editor: Editor)
case class Lock(paragraph: Paragraph, editor: Editor)
case class Unlock(paragraph: Paragraph, editor: Editor)
case class HasLock(paragraph: Paragraph, editor: Editor)
case class IsLocked(paragraph: Paragraph)
case class Locked
case class Unlocked
