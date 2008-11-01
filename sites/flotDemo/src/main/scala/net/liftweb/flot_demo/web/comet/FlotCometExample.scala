
package net.liftweb.flot_demo.web.comet

import scala.xml.{NodeSeq, Text}

import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.Can

import net.liftweb.widgets.flot._

import net.liftweb.flot_demo.web.model._

/*
 *
 */

class FlotCometExample
(
  theSession : LiftSession,
  name: Can[String],
  defaultXml: NodeSeq,
  attributes: Map[String, String]
)
  extends CometActor(theSession, name, defaultXml, attributes)
{
  var options : FlotOptions = new FlotOptions
  var series : List [FlotSerie] = Nil
  val idPlaceholder = "ph_graph"

  def defaultPrefix = "flot"

  def render = {
    bind("flot", "graph" -> Flot.render ("ph_graph", series, options, Text ("")))
  }

  //

  override def localSetup {
    Sensor.acum !? AddListener(this) match {
      case flotInfo : FlotInfo => {
        options = flotInfo.options
        series = flotInfo.series
      }
    }
  }

  def logFlotSerie = {
    for (serie <- series) {
      println (serie.label)
      println (serie.data)
    }
  }

  override def localShutdown {
    Sensor.acum ! RemoveListener(this)
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case newData : FlotNewData => {
      //
      series = newData.series

      val listData = newData.datas
      val js = JsFlotAppendData (idPlaceholder,
                                 series,
                                 listData,
                                 true)

      partialUpdate (js)
    }
  }
}
