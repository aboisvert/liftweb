package net.liftweb.widgets.menu

import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text, Unparsed}
import _root_.net.liftweb.http.{LiftRules, S}
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.sitemap._
import JsCmds._
import JE._
import _root_.net.liftweb.util._
import Helpers._

object MenuStyle extends Enumeration("sf-menu", "sf-menu sf-vertical", "sf-menu sf-navbar") {
  val HORIZONTAL, VERTICAL, NAVBAR = Value
}

object MenuWidget {

  def apply() = new MenuWidget(LiftRules.siteMap open_!, MenuStyle.HORIZONTAL, JsObj()) render

  def apply(style: MenuStyle.Value) = new MenuWidget(LiftRules.siteMap open_!, style, JsObj()) render

  def apply(siteMap: SiteMap) = new MenuWidget(siteMap, MenuStyle.HORIZONTAL, JsObj()) render

  def apply(siteMap: SiteMap, style: MenuStyle.Value) = new MenuWidget(siteMap, style, JsObj()) render

  def apply(jsObj: JsObj) = new MenuWidget(LiftRules.siteMap open_!, MenuStyle.HORIZONTAL, jsObj) render

  def apply(style: MenuStyle.Value, jsObj: JsObj) = new MenuWidget(LiftRules.siteMap open_!, style, jsObj) render

  def apply(siteMap: SiteMap, jsObj: JsObj) = new MenuWidget(siteMap, MenuStyle.HORIZONTAL, jsObj) render

  def apply(siteMap: SiteMap, style: MenuStyle.Value, jsObj: JsObj) = new MenuWidget(siteMap, style, jsObj) render


   /**
    * register the resources with lift (typically in boot)
    */
  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "menu" :: _ => true
     })
  }

}

/**
 * Builds a Menu widget based on a give SiteMap
 */
class MenuWidget(siteMap: SiteMap, style: MenuStyle.Value, jsObj: JsObj) {
  def head: NodeSeq = <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish.css"} type="text/css"/>{
        style match {
	      case MenuStyle.VERTICAL =>  <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish-vertical.css"} type="text/css"/>
	      case MenuStyle.NAVBAR =>  <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/menu/superfish-navbar.css"} type="text/css"/>
          case _ => NodeSeq.Empty
	    }
      }
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/menu/superfish.js"}></script>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/menu/jquery.hoverIntent.js"}></script>
      <script type="text/javascript" charset="utf-8">{
        Unparsed("""
         jQuery(document).ready(function() {
            jQuery('ul.sf-menu').superfish(""" + jsObj.toJsCmd + """);
          })
         """)
       }
      </script>
    </head>


  def render : NodeSeq = {
    head ++ <lift:Menu.builder expandAll="true" top:class={style.toString} />
  }

}
