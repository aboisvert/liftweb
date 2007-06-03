package com.liftweb.wiki.controller

import scala.xml._
import net.liftweb.http.{Page => HTTPPage, _}
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.mapper._
import com.liftweb.wiki.model._
import com.liftweb.wiki.actor._

class Editor extends ControllerActor {
  private val manager = LockManager.instance
  private var paragraph: Option[Paragraph] = None

  override def localSetup {
    paragraph = Paragraph.find(name)
    paragraph match {
      case Some(paragraph) => manager ! Register(paragraph, this)
      case None =>
    }
  }

  def render = paragraph match {
    case Some(paragraph) =>
      val lockDummy = this.uniqueId + "_id"
      S.addFunctionMap(lockDummy, &lock)
      val unlockDummy = this.uniqueId + "_id"
      S.addFunctionMap(unlockDummy, &unlock)
      val text = this.uniqueId + "_id"
      S.addFunctionMap(text, &save)

      val hasLock: Boolean = manager !? HasLock(paragraph, this) match {
        case result: Boolean => result
      }
      val isLocked: Boolean = manager !? IsLocked(paragraph) match {
        case result: Boolean => result
      }

      <li>
        <h3>{ paragraph.title }, { hasLock.toString }, { isLocked.toString }</h3>
        <lift:form method="POST" action=".">
          {
            if (hasLock) <textarea name={ text } class="owned">{ paragraph.text }</textarea>
            else if (isLocked) <textarea name={ text } class="locked">{ paragraph.text }</textarea>
            else <textarea name={ text } class="unlocked">{ paragraph.text }</textarea> 
          }
          {
            if (hasLock) <input type="submit" value="save" />
            else "foo"
          }
        </lift:form>
        { if (!isLocked || hasLock) {
            <lift:form method="POST" action=".">
              <input name={ if (hasLock) unlockDummy else lockDummy } value="" type="hidden"/>
              <input type="submit" value={ if (hasLock) "unlock" else "lock" } />
            </lift:form>
          } else {
            "bar"
          }
        }
      </li>

    case None =>
      <i>No paragraph to edit</i>
  }

  override def lowPriority: PartialFunction[Any, Unit] = {
    val ret: PartialFunction[Any, Unit] = {
      case Locked() | Unlocked() => reRender
      case ('EXIT, _) => terminate
    }
    ret orElse super.lowPriority
  }

  private def lock(in: List[String]) = {
    Console.println(this + " try to lock")
    paragraph match {
      case Some(paragraph) =>
        manager ! Lock(paragraph, this)
      case None =>
    }
    true
  }

  private def unlock(in: List[String]) = {
    paragraph match {
      case Some(paragraph) =>
        manager !? Unlock(paragraph, this)
      case None =>
    }
    true
  }

  private def save(in: List[String]) = {
    manager !? HasLock(paragraph, this) match {
      case true =>
        val r = new Revision
        r.text := in.head
        paragraph.addRevision(r)
      case false =>
    }
    true
  }

  private def terminate {
    manager ! Unregister(paragraph, this)
    manager ! Unlock(paragraph, this)
  }

}
