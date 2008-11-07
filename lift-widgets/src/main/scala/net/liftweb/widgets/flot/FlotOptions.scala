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

/**
 * axis options
 */

class FlotAxisOptions {
  val tickDecimals: Option [Double] = None
  val min: Option [Double] = None
  val max: Option [Double] = None
  val mode: Option [String] = None
  val ticks: List [Double] = Nil //  null or number or ticks array or (fn: range -> ticks array)

  /* TODO
    autoscaleMargin: null or number
    labelWidth: null or number
    labelHeight: null or number
    tickSize: number or array
    minTickSize: number or array
    tickFormatter: (fn: number, object -> string) or string
    */
}

/**
 * legend options
 */

class FlotLegendOptions {
  val show: Option [Boolean] = None
  val labelFormatter : Option [String] = None // null or (fn: string -> string)
  val labelBoxBorderColor: Option [String] = None // color
  val noColumns: Option [Int] = None // number
  val position: Option [String] = None // "ne" or "nw" or "se" or "sw"
  val margin: Option [Int] = None // number of pixels
  val backgroundColor: Option [String] = None //  null or color
  val backgroundOpacity: Option [Double] = None // number in 0.0 - 1.0
  val container: Option [String] = None // null or jQuery object
}

/**
 * lines options and points/bars options parent
 */

class FlotLinesOptions {
  val show: Option [Boolean] = None
  val lineWidth : Option [Int] = None
  val fill : Option [Boolean] = None // TODO: boolean or number
  val fillColor : Option [String] = None
}

class FlotPointsOptions extends FlotLinesOptions {
  val radius : Option [Int] = None  // TODO
}

class FlotBarsOptions extends FlotLinesOptions {
  val barWidth : Option [Int] = None // TODO
}

/**
 * grid options
 */

class FlotGridOptions {
  val color: Option [String] = None
  val backgroundColor: Option [String] = None
  val tickColor: Option [String] = None
  val labelMargin: Option [Int] = None
  val coloredAreasColor: Option [String] = None
  val borderWidth: Option [Int] = None
  val clickable: Option [Boolean] = None
  val coloredAreas: Option [String] = None // only (fn: plot area -> array of areas)

  /* TODO
  coloredAreas: array of areas or (fn: plot area -> array of areas)
  */
}

/**
 * Options
 */

class FlotOptions {
  val lines : Option [FlotLinesOptions] = None
  val points : Option [FlotPointsOptions] = None
  val legend: Option [FlotLegendOptions] = None
  val xaxis: Option [FlotAxisOptions] = None
  val yaxis: Option [FlotAxisOptions] = None
  val modeSelection: Option [String] = None
  val shadowSize: Option [Int] = None
  val grid: Option [FlotGridOptions] = None
}


