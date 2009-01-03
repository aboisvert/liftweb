/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.widgets.flot

import scala.xml.{NodeSeq, Node, PCData, Text, Unparsed}

/**
 * create an overview graph to help zooming in the main graph
 */

class FlotOverview (idOverview : String, optionsOverview : FlotOptions)
  extends FlotCapability {

  def render (main : FlotInfo) : String = {
    val sz = new StringBuilder (1000)

    sz.append ("  var options_" + idOverview + " = " + Flot.renderOptions (optionsOverview) + " ;\n\n")
    sz.append ("  var plot_" + idOverview +" = jQuery.plot(jQuery('#" + idOverview + "'), datas_" + main.idPlaceholder + ", options_" + idOverview + ") ; \n\n")

    sz.append ("  var internalSelection = false;\n")

    sz.append ("  jQuery('#" + main.idPlaceholder + "').bind('selected', function (event, area) { \n")
    sz.append ("    plot_" + main.idPlaceholder + " = jQuery.plot(jQuery('#" + main.idPlaceholder + "'), datas_" + main.idPlaceholder + ",\n")
    sz.append ("                jQuery.extend(true, {}, options_" + main.idPlaceholder + ", {\n")
    sz.append ("                    xaxis: { min: area.x1, max: area.x2 },\n")
    sz.append ("                    yaxis: { min: area.y1, max: area.y2 }\n")
    sz.append ("                }));\n\n")

    sz.append ("    if (internalSelection)\n")
    sz.append ("        return;\n")
    sz.append ("    internalSelection = true;\n")
    sz.append ("    plot_" + idOverview +".setSelection(area);\n")
    sz.append ("    internalSelection = false;\n")
    sz.append ("  });\n\n")

    sz.append ("  jQuery('#" + idOverview +"').bind('selected', function (event, area) {\n")
    sz.append ("      if (internalSelection)\n")
    sz.append ("          return;\n")
    sz.append ("      internalSelection = true;\n")
    sz.append ("      plot_" + main.idPlaceholder + ".setSelection(area);\n")
    sz.append ("      internalSelection = false;\n")
    sz.append ("  });\n")


    sz.toString
  }

  def renderHide () = {
    "jQuery(\"#" + idOverview + "\").hide () ; \n"
  }

  def renderShow () = {
    "jQuery(\"#" + idOverview + "\").show () ; \n"
  }
}

