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

import net.liftweb.http.js.JsCmd


/**
 *
 */

case class JsFlot (val idPlaceholder : String, val datas : List [FlotSerie], val options : FlotOptions) extends JsCmd
{
  def toJsCmd: String = {
    Flot.renderJs (idPlaceholder, datas, options, Nil)
  }
}

/**
 *
 */

case class JsFlotAppendData (val idPlaceholder : String, val datas : List [FlotSerie], val newDatas : List [Pair [Double, Double]], pop : Boolean) extends JsCmd
{
  def toJsCmd: String = {
    if (datas.size != newDatas.size)
      throw new Exception ("data are diferrent zize")

    var num = 0
    val newValuePush =
      newDatas.map (newData => {
          num = num + 1 ;
          val nameSerie = "data_" + idPlaceholder + "_" + num
          val popjs = if (pop) {nameSerie + ".shift () ;\n"} else ""

          popjs + nameSerie + ".push ( [" + newData._1.toString + ", " + newData._2.toString + "]) \n"
        }
      ).reduceLeft ((x : String, y : String) => x + y)

    val flotShow = Flot.renderFlotShow (idPlaceholder, datas, new FlotOptions, Nil)

    newValuePush + flotShow
  }
}

/**
 *
 */

case class JsFlotWithOverview (val idPlaceholder : String,
                               val datas : List [FlotSerie],
                               val options : FlotOptions,
                               val idOverview : String,
                               val optionsOverview : FlotOptions) extends JsCmd
{
  def toJsCmd: String = {
    val jsClearLegend = optionsOverview.legend match {
      case Some (flotLegendOptions) => {
        flotLegendOptions.container match {
          case Some (phContainer) => "    jQuery(\"#" + phContainer + "\").html (\"\") ;\n"
          case None => ""
        }
      }
      case None => ""
    }

    val overview = new FlotOverview (idOverview, optionsOverview)

    jsClearLegend + Flot.renderJs (idPlaceholder, datas, options, Nil, overview)
  }
}


