package net.liftweb.flot_demo.web.snippet

import net.liftweb.widgets.flot._
import _root_.net.liftweb.util._

object Constants
{
  val options = new FlotOptions () {
        override val xaxis = Full (new FlotAxisOptions () {
          override val mode = Full ("time")
        })
      }

  val sdf = new java.text.SimpleDateFormat ("yyyy.MM.dd")
}


