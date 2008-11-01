package net.liftweb.flot_demo.web.snippet

import scala.xml.NodeSeq

import net.liftweb.util.Helpers._

import net.liftweb.widgets.flot._

/*
 *
 */

class SettingOption {

  def render (xhtml: NodeSeq) = {

    def graph () = {

      var d1 : List [Pair [Double, Double]] = Nil
      var d2 : List [Pair [Double, Double]] = Nil
      var d3 : List [Pair [Double, Double]] = Nil

      var i = 0.0
      while (i < Math.Pi * 2.0)
      {
        d1 = (i, Math.sin(i)) :: d1
        d2 = (i, Math.cos(i)) :: d2
        d3 = (i, Math.tan(i)) :: d3

        i += 0.25
      }

      val s1 = new FlotSerie () {
        override val data = d1
        override val label = Some ("sin(x)")
      }

      val s2 = new FlotSerie () {
        override val data = d2
        override val label = Some ("cos(x)")
      }

      val s3 = new FlotSerie () {
        override val data = d3
        override val label = Some ("tan(x)")
      }

      val options = new FlotOptions () {
        override val lines = Some (new FlotLinesOptions () {
          override val show = Some (true)
        })
        override val points = Some (new FlotPointsOptions () {
          override val show = Some (true)
        })
        // TODO x-axis
        override val yaxis = Some (new FlotAxisOptions () {
          override val ticks = 10.0 :: Nil
          override val min = Some (-2.0)
          override val max = Some (2.0)
        })
        override val grid = Some (new FlotGridOptions () {
          override val backgroundColor = Some ("#fffaff")
        })
      }

      Flot.render ( "ph_graph", s3 :: s2 :: s1 :: Nil, options)
    }

    //

    bind ("flot", xhtml, "graph" --> graph)
  }
}
