package net.liftweb.flot_demo.web.snippet

import scala.xml.NodeSeq

import net.liftweb.util.Helpers._

import net.liftweb.widgets.flot._

/*
 *
 */

class Zooming {

  def getData (x1 : Double, x2 : Double) : List [Pair [Double, Double]] = {
    var ret : List [Pair [Double, Double]] = Nil

    var i = x1
    while (i < x2) {
      ret = (i, Math.sin (i * Math.sin(i))) :: ret
      i += (x2 - x1) / 100
    }

    ret
  }

  def render (xhtml: NodeSeq) = {

    /*
    */

    def graph () = {
      val data = new FlotSerie () {
        override val label = Some ("sin(x sin(x))")
        override val data = getData (0.0, 3 * Math.Pi)
      }

      val options = new FlotOptions () {
        override val lines = Some(new FlotLinesOptions () {
          override val show = Some (true)
        })
        override val points = Some(new FlotPointsOptions () {
          override val show = Some (true)
        })
        override val yaxis = Some (new FlotAxisOptions () {
          override val ticks = 10.0 :: Nil
        })
        override val legend = Some (new FlotLegendOptions () {
          override val show = Some (false)
        })
        override val modeSelection = Some ("xy")
      }

      val optionsOverview = new FlotOptions () {
        override val lines = Some(new FlotLinesOptions () {
          override val show = Some (true)
          override val lineWidth = Some (1)
        })
        override val legend = Some (new FlotLegendOptions () {
          override val show = Some (true)
          override val container = Some ("ph_legend")
        })
        override val grid = Some (new FlotGridOptions () {
          override val color = Some ("#999")
        })
        override val shadowSize = Some (0)
        override val xaxis = Some (new FlotAxisOptions () {
          override val ticks = 4.0 :: Nil
        })
        override val yaxis = Some (new FlotAxisOptions () {
          override val ticks = 3.0 :: Nil
          override val min = Some (-2.0)
          override val max = Some (2.0)
        })
        override val modeSelection = Some ("xy")
      }

      val overview = new FlotOverview ("ph_overview", optionsOverview)

      Flot.render ("ph_graph", data :: Nil, options, overview)
    }

    //

    bind ("flot", xhtml, "graph" --> graph)
  }
}
