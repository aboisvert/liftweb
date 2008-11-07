/*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
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
 * renders a flot graph using http://code.google.com/p/flot/ jQuery widget
 * <br />
 * See the sites/flotDemo webapp for examples.
 */

object Flot
{
  /**
   * register the resources with lift (typically in boot)
   */

  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
      case "flot" :: "jquery.flot.js" :: Nil => true
      case "flot" :: "excanvas.pack.js" :: Nil => true
    })
    println("register flot")
  }

  /**
   * render a Flot Graph
   * <p>
   * search extra javascript in xhtml templates
   */

  def render (idPlaceholder : String,
              datas : List[FlotSerie],
              options : FlotOptions,
              caps : FlotCapability*
              ) : (NodeSeq => NodeSeq) = {

    def ret (in : NodeSeq) : NodeSeq = {

      // search for a script tag in the template
      val tagScript = in \\ "script"
      val jqueryScript = if (! tagScript.isEmpty) tagScript (0).child else Text ("")

      render (idPlaceholder, datas, options, jqueryScript, caps :_*)
    }

    ret
  }

  /**
   * render a flot graph
   * <p>
   * a comet actor should use this version
   */

  def render (idPlaceholder : String,
              datas : List[FlotSerie],
              options : FlotOptions,
              jqueryScript : NodeSeq,
              caps : FlotCapability*
              ) : NodeSeq =
  {
      val ieExcanvasPackJs = Unparsed ("<!--[if IE]><script language=\"javascript\" type=\"text/javascript\" src=\"" + net.liftweb.http.S.contextPath + "/classpath/flot/excanvas.pack.js\"></script><![endif]-->")

      //
      <xml:group>
      <head>
        <script type="text/javascript" src="/classpath/flot/jquery.flot.js"></script>
        {ieExcanvasPackJs}
        <script type="text/javascript" charset="utf-8">
{_renderJs (idPlaceholder, datas, options, jqueryScript, caps :_*)}
        </script>
      </head>
      </xml:group>
  }
  /*
   *
   */

  def renderCapability (fRender : FlotCapability => String, caps : FlotCapability *) : String =
  {
    if (caps.size  == 0)
      ""
    else
      caps.map (c => fRender (c)).reduceLeft ((x : String, y : String) => x + y)
  }

  /*
   * can be used to generate AJAX response
   */

  def renderJs (
                 idPlaceholder : String,
                 datas : List [FlotSerie],
                 options : FlotOptions,
                 jqueryScript : Seq [Node],
                 caps : FlotCapability *
               ) = {
    val js = datas match {
      case Nil => renderFlotHide (idPlaceholder, caps: _*)

      case _ => renderVars (idPlaceholder, datas, options) + "\n" +
                renderFlotShow (idPlaceholder, datas, options, jqueryScript, caps :_*)
    }

    js
  }

  //

  def renderFlotHide (idPlaceholder : String , caps : FlotCapability *) : String = {
    "jQuery(\"#" + idPlaceholder + "\").hide () ;\n" +
                            renderCapability (c => c.renderHide (), caps :_*)
  }

  // part that belongs to jQuery "document ready" function

  def renderFlotShow (
                 idPlaceholder : String,
                 datas : List [FlotSerie],
                 options : FlotOptions,
                 jqueryScript : Seq [Node],
                 caps : FlotCapability *
               ) : String = {

    val main = FlotInfo (idPlaceholder, datas, options)

    "jQuery(\"#" + idPlaceholder + "\").show () ;\n" +
      renderCapability (c => c.renderShow (), caps :_*) + "\n" +
      "var plot_" + idPlaceholder +
      " = jQuery.plot(jQuery(" + renderId (idPlaceholder) +
      "), datas_" + idPlaceholder +
      ", options_" + idPlaceholder + ");\n" +
      renderCapability (c => c.render (main), caps :_*) +
      renderJqueryScript (jqueryScript)
 }

  // generate Javascript inside "document ready" event

  private def _renderJs (
                        idPlaceholder : String,
                        datas : List [FlotSerie],
                        options : FlotOptions,
                        jqueryScript : Seq [Node],
                        caps : FlotCapability*
                       ) = {

    // see http://scala.sygneca.com/faqs/xml

    val js = "/* <![CDATA[ */\n" +
             renderVars (idPlaceholder, datas, options) + "\n" + // render the data outside jQuery document ready function
             initFlot + "\n" +
             (datas match {
               case Nil => renderFlotHide (idPlaceholder, caps : _*)
               case _ => renderFlotShow (idPlaceholder, datas, options, jqueryScript, caps : _*)
             }) +
             endFlot + "\n" +
             "/* ]]> */\n"

    Unparsed (js)
  }

  private def renderJqueryScript (jqueryScript : Seq [Node]) : String = {
    jqueryScript.foldLeft ("") ( (sz,node) => {
      sz + (node match {
          case net.liftweb.util.PCData (_s) => _s
          case _ => node.toString
        })
    })
  }

  //

  val initFlot = "jQuery(function () {"
  val endFlot = "});"

  /**
   * render a data value:<br/>
   * [2, 10]
   */
  def renderOneValue (one : Pair [Double, Double]) : String = {
    one match {
      case (Math.NaN_DOUBLE, _) => "null"
      case (_, Math.NaN_DOUBLE) => "null"
      case _one => "[" + _one._1 + ", " + _one._2 + "]"
    }
  }

  /**
   * render serie of data:<br/>
   * [2, 10], [5, 12], [11, 2]
   */
  def renderValues (values : List [Pair [Double, Double]]) : String = {
    values match {
      case List (last) => renderOneValue (last)
      case head :: tail => renderOneValue (head) + ", " + renderValues (tail)
    }
  }

  /**
   *
   */

  def renderDataSerie (data : FlotSerie, idPlaceholder : String, num : Int) : String = {
    "var data_" + idPlaceholder + "_" + num + " = [" + renderValues (data.data) + "] ;\n"
  }

  /*
   * render all variables that can be modified via Javascript after first page load (for example using Ajax or comet)
   */

  def renderVars (
                   idPlaceholder : String,
                   datas : List [FlotSerie],
                   options : FlotOptions
                  ) : String = {

    datas match {
      case Nil => ""

      case head :: tail => {
        var num = 0

        val series = datas.map (serie => {num = num + 1; renderDataSerie (serie, idPlaceholder, num)})

        series.reduceLeft ((x : String, y : String) => x + y) + "\n" +
          "var datas_" + idPlaceholder + " = [\n" +
          renderSeries (datas, idPlaceholder, 1) + "\n" +
          "] ;\n" +
          "var options_" + idPlaceholder + " = " + renderOptions (options) + "; \n"
      }
    }
  }



  /**
   * render one serie:<br />
   * <br />
   * <code>
   * (
   *   label: "<name_label>"
   *   lines: { show: true, fill: true }
   *   bars: { show: true }
   *   points: { show: true }
   *   data: data_[ph]_[x] where ph is the placeholder id and {x} serie's the id
   * )
   * </code>
   */

  def renderOneSerie (data : FlotSerie, idPlaceholder : String, idSerie : Int) : String = {
    val set_label = data.label match {
      case Some (label) =>
        "      label: '" + label + "',\n" ;
      case _ => ""
    }

    //
    val set_lines = data.lines match {
      case None => ""
      case Some (_lines) => "      lines: {" + renderLines (_lines) + "},\n"
    }

    val set_points = data.points match {
      case None => ""
      case Some(_points) => "      points: {" + renderPoints (_points) + "},\n"
    }

    val set_bars = data.bars match {
      case None => ""
      case Some(_bars) => "      bars: {" + renderBars (_bars) + "},\n"
    }

    val set_color = data.color match {
      case None => ""
      case Some (Left (_color)) => "      color: '" + _color + "',\n"
      case Some (Right (_color)) => "      color: " + _color + ",\n"
    }

    val set_shadow_size = data.shadowSize match {
      case None => ""
      case Some (_shadowSize) => "      shadowSize: " + _shadowSize + ",\n"
    }

    "    {\n" +
           set_label +
           set_lines +
           set_points +
           set_bars +
           set_color +
           set_shadow_size +
    "      data: data_" + idPlaceholder + "_" + idSerie + "\n" +
    "    }"
  }

  /**
   * render all series: <br />
   * <br />
   * ( <br />
   *   label: "<name_label_1>" <br />
   *   lines:  ... <br />
   *   data: [[2, 10], [5, 12], [11, 2]] <br />
   * ), <br />
   * (<br />
   *   label: "<name_label2>"<br />
   *   data: [[2, 14], [6, 4], [11, 17]]<br />
   * )<br />
   *
   */
  def renderSeries (_datas : List [FlotSerie], idPlaceholder : String, idSerie : Int) : String = {

    _datas match {
      case List(last) => renderOneSerie (last, idPlaceholder, idSerie)
      case head :: tail => renderOneSerie (head, idPlaceholder, idSerie) + ",\n" + renderSeries (tail, idPlaceholder, idSerie + 1)
    }
  }

  //

  def renderTicks (ticks : List [Double]) : String = {
    "TO-DO"
  }

  //
  // min: 0, max: 10, tickDecimals: 0
  // mode: "time",
  // minTickSize: [1, "month"],    // TODO
  //
  def renderAxisOptions (options : FlotAxisOptions) : String = {
    var sz = ""
    var coma = ""

    sz = options.min match {
      case None => ""
      case Some (_min) => {coma = ", "; "min: " + _min}
    }

    sz += (options.max match {
      case None => ""
      case Some (_max) => {val ret = coma + "max: " + _max; coma = ", "; ret}
    })

    sz += (options.tickDecimals match {
      case None => ""
      case Some (_tickDecimals) => {val ret = coma + "tickDecimals: " + _tickDecimals; coma = ", "; ret}
    })

    sz += (options.ticks match {
      case Nil => ""
      case List (_one) => {val ret = coma + "ticks: " + _one; coma = ", "; ret}
      case head :: tail => {val ret = coma + "ticks: " + renderTicks (options.ticks); coma = ", "; ret}
    })

    sz += (options.mode match {
      case None => ""
      case Some (_mode) => {val ret = coma + "mode: '" + _mode + "'"; coma = ", "; ret}
    })

    sz
  }

  //
  //    xaxis: { tickDecimals: 0 },
  //    yaxis: { min: 0, max: 10 },
  //
  def renderAxis (axis : String, options : FlotAxisOptions) : String = {
    axis + "axis: {" + renderAxisOptions (options) + "}"
  }

  //
  //
  //

  def renderGrid (grid : FlotGridOptions) : String = {
    val sz : StringBuilder = new StringBuilder (200)
    var coma = ""

    sz.append (grid.color match {
      case None => ""
      case Some(_color) => {val ret = "color: '" + _color + "'"; coma = ","; ret}
    })

    sz.append (grid.backgroundColor match {
      case None => ""
      case Some(_backgroundColor) => {val ret = coma + "backgroundColor: '" + _backgroundColor + "'"; coma = ","; ret}
    })

    sz.append (grid.tickColor match {
      case None => ""
      case Some(_tickColor) => {val ret = coma + "tickColor: '" + _tickColor + "'"; coma = ","; ret}
    })

    sz.append (grid.labelMargin match {
      case None => ""
      case Some(_labelMargin) => {val ret = coma + "labelMargin: " + _labelMargin; coma = ","; ret}
    })

    sz.append (grid.coloredAreasColor match {
      case None => ""
      case Some(_coloredAreasColor) => {val ret = coma + "coloredAreasColor: '" + _coloredAreasColor + "'"; coma = ","; ret}
    })

    sz.append (grid.borderWidth match {
      case None => ""
      case Some(_borderWidth) => {val ret = coma + "borderWidth: " + _borderWidth; coma = ","; ret}
    })

    sz.append (grid.clickable match {
      case None => ""
      case Some(_clickable) => {val ret = coma + "clickable: " + _clickable; coma = ","; ret}
    })

    sz.append (grid.coloredAreas match {
      case None => ""
      case Some(_coloredAreas) => {val ret = coma + "coloredAreas: " + _coloredAreas; coma = ","; ret}
    })

    sz.toString
  }

  //
  //
  //

  def renderLegend (legend : FlotLegendOptions) : String = {
    val sz : StringBuilder = new StringBuilder (200)
    var coma = ""

    sz.append (legend.labelFormatter match {
      case None => ""
      case Some(_labelFormatter) => {coma = ", "; _labelFormatter}
    })

    sz.append (legend.show match {
      case None => ""
      case Some(_show) => {val ret = coma + "show: " + _show ; coma = ", "; ret}
    })

    sz.append (legend.labelBoxBorderColor match {
      case None => ""
      case Some(_labelBoxBorderColor) => {val ret = coma + "labelBoxBorderColor: " + _labelBoxBorderColor ; coma = ", "; ret}
    })

    sz.append (legend.noColumns match {
      case None => ""
      case Some(_noColumns) => {val ret = coma + "noColumns: " + _noColumns ; coma = ", "; ret}
    })

    sz.append (legend.position match {
      case None => ""
      case Some(_position) => {val ret = coma + "position: " + _position ; coma = ", "; ret}
    })

    sz.append (legend.margin match {
      case None => ""
      case Some(_margin) => {val ret = coma + "margin: " + _margin ; coma = ", "; ret}
    })

    sz.append (legend.backgroundColor match {
      case None => ""
      case Some(_backgroundColor) => {val ret = coma + "backgroundColor: " + _backgroundColor ; coma = ", "; ret}
    })

    sz.append (legend.backgroundOpacity match {
      case None => ""
      case Some(_backgroundOpacity) => {val ret = coma + "backgroundOpacity: " + _backgroundOpacity ; coma = ", "; ret}
    })

    sz.append (legend.container match {
      case None => ""
      case Some(_container) => {val ret = coma + "container: jQuery('#" + _container + "')"; coma = ", "; ret}
    })

    sz.toString
  }

  //
  // lines: { show: true, lineWidth: 1
  //

  def renderLines (lines : FlotLinesOptions) : String = {
    val sz = new StringBuilder (200)
    var coma = ""

    sz.append (lines.show match {
      case None => ""
      case Some(_show) => {val ret = coma + "show: " + _show; coma = ", "; ret}
    })

    sz.append (lines.lineWidth match {
      case None => ""
      case Some(_lineWidth) => {val ret = coma + "lineWidth: " + _lineWidth; coma = ", "; ret}
    })

    sz.append (lines.fill match {
      case None => ""
      case Some(_fill) => {val ret = coma + "fill: " + _fill; coma = ", "; ret}
    })

    sz.append (lines.fillColor match {
      case None => ""
      case Some(_fillColor) => {val ret = coma + "fillColor: '" + _fillColor + "'"; coma = ", "; ret}
    })

    sz.toString
  }

  //
  //
  //

  def renderPoints (points : FlotPointsOptions) : String = {
    renderLines (points)

    // TO-DO radius
  }

  def renderBars (bars : FlotBarsOptions) : String = {
    renderLines (bars)

    // TO-DO rest of bar
  }

  //
  // {
  //    lines: { show: true},
  //    points: { show: true}
  //    xaxis: { tickDecimals: 0 },
  //    yaxis: { min: 0, max: 10 },
  //    selection: { mode: "x" }
  //    legend: { noColumns: 2 },
  // }
  //
  def renderOptions (options : FlotOptions) : String = {
    var first = true

    def endOfLine () = {
      val ret = if (! first) ",\n      " else "      "
      first = false
      ret
    }

    val set_lines = options.lines match {
      case None => ""
      case Some (_lines) => {first=false; "lines: {" + renderLines (_lines) + "}"}
    }

    val set_points = options.points match {
      case None => ""
      case Some (_points) => {endOfLine + "points: {" + renderPoints (_points) + "}"}
    }

    val set_xaxis = options.xaxis match {
      case None => ""
      case Some (options) => {endOfLine + renderAxis ("x", options)}
    }

    val set_yaxis = options.yaxis match {
      case None => ""
      case Some (options) => {endOfLine + renderAxis ("y", options)}
    }

    val set_selection = options.modeSelection match {
      case None => ""
      case Some (mode) => {endOfLine + "selection: { mode: '" + mode + "'}"}
    }

    val set_legend = options.legend match {
      case None => ""
      case Some (_legend) => {endOfLine + "legend: {" + renderLegend (_legend) +  "}"}
    }

    val set_shadowSize = options.shadowSize match {
      case None => ""
      case Some (_shadowSize) => {endOfLine + "shadowSize: " + _shadowSize}
    }

    val set_grid = options.grid match {
      case None => ""
      case Some (_grid) => {endOfLine + "grid: {" + renderGrid (_grid) + "}"}
    }

    if (! first)
    {
      "{\n" +
        set_lines +
        set_points  +
        set_xaxis +
        set_yaxis +
        set_selection +
        set_legend +
        set_shadowSize +
        set_grid +
      "  }"
    }
    else
       "{}"
  }

  def renderId (id : String) : String = {
    "'#" + id + "'"
  }

}


