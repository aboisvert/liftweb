package com.liftweb.wiki.snippet

import scala.xml._
import net.liftweb.http.{Page => HTTPPage, _}
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.mapper._
import com.liftweb.wiki.model._

/** Snippet handling the Wiki pages.
 */
class Wiki {

  /** Display a page.
   */
  def page(xhtml: Group) = 
    S.param("page") match {
      case Some(title) =>
        Page.find(By(Page.title, title)) match {
          case Some(p) => 
            <div id="page">
              { links(p.title) }
              { p.html }
            </div>
          case None =>
            <div id="page"> {
              if (logged) redirectTo("/create?page=" + title)
              else <span>You have to be logged in to create a new page</span>
            } </div>
        }
      case None =>
        Page.find(By(Page.title, "index")) match {
          case Some(p) => 
            <div id="page">
              { links(p.title) }
              { p.html }
            </div>
          case None => 
            <div id="page"> {
              if (logged) redirectTo("/create?page=index")
              else <span>You have to be logged in to create a new page</span>
            } </div>
        }
    }

  /** Returns the edit and delete links for a specified page.
   */
  private def links(page: String) = {
    val editLink = "/edit?page=" + page
    val deleteLink = "/delete?page=" + page
    if (logged) <span><a href={ editLink }>Edit</a> | <a href={ deleteLink }>Delete</a></span>
    else <span>You have to be logged in to modify this page</span>
  }

  /** Create a new page.
   */
  def create(xhtml: Group) =
    if (!logged) {
      S.error("can't add a new page if you're not logged in")
      S.redirectTo("/")
    } else {
      S.param("page") match {
        case Some(title) =>
          val page = new Page
          page.title := title
          def doCreate(in: List[String]): Boolean = {
            val issues = page.validate

            if (issues.isEmpty && Page.findAll(By(Page.title, title)).isEmpty) {
              page.save
              S.notice("page created")
              S.redirectTo("/")
            } else {
              S.error("issues while creating a page, a page with this title may already exist")
            }
        
            true
          }
          <form method="POST" action={ S.request.uri }>
            { page.toForm(doCreate) }
            <input type="submit" value="Create new page" />
          </form>

        case None =>
          S.error("no page to create")
          S.redirectTo("/")
      }
    }

  /** Instanciate Editor controllers for all paragraphs of the specified page.
   */
  def edit(xhtml: Group) = {
    S.param("page") match {
      case Some(title) =>
        Page.find(By(Page.title, title)) match {
          case Some(p) =>
            <ol>
              { for (val paragraph <- p.paragraphs) yield
                <lift:controller type="Editor" name={ paragraph.id.get.toString }>
                  Editor for paragraph { paragraph.id.get.toString }
                </lift:controller>
              }
              <li>
                <a href={ "/create_paragraph?page=" + p.title }>Append a new paragraph</a>
              </li>
            </ol>
          
          case None =>
            S.error("invalid page to edit")
            S.redirectTo("/")
        }

      case None =>
        S.error("no page title to edit")
        S.redirectTo("/")
    }
  }

  def createParagraph(xhtml: Group): NodeSeq = {
    if (!logged) {
      S.error("can't create a paragraph if you're not logged in")
      S.redirectTo("/")
    } else {
      S.param("page") match {
        case Some(title) =>
          Page.find(By(Page.title, title)) match {
            case Some(p) => 
              val paragraph = new Paragraph 
              def doCreate(in: List[String]): Boolean = {
                val issues = paragraph.validate

                if (issues.isEmpty) {
                  p.appendParagraph(paragraph)
                  S.notice("paragraph created")
                  S.redirectTo("/edit?page=" + S.param("page").get)
                } else {
                  S.error("issues while creating a paragraph")
                }
        
                true
              }
              <form method="POST" action={ S.request.uri + "?page=" + S.param("page").get }>
                { paragraph.toForm(doCreate) }
              <input type="submit" value="Create new paragraph" />
              </form>      
            case None =>
              S.error("page not found")
              S.redirectTo("/")
          }  
        case None =>
          S.error("page title not found")
          S.redirectTo("/")
      }
    }
  }

  private def logged = S.get("username").isDefined

}
