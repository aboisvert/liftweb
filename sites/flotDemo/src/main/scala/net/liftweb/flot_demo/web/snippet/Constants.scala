package net.liftweb.flot_demo.web.snippet

import net.liftweb.widgets.flot._

object Constants
{
  val options = new FlotOptions () {
        override val xaxis = Some (new FlotAxisOptions () {
          override val mode = Some ("time")
        })
      }

  val sdf = new java.text.SimpleDateFormat ("yyyy.MM.dd")
}


