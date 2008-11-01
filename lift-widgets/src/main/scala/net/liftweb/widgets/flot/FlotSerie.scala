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
 * Serie of data
 */

class FlotSerie
{
  val data: List [Pair [Double, Double]] = Nil
  val label : Option [String] = None
  val lines : Option[FlotLinesOptions] = None
  val points : Option[FlotPointsOptions] = None
  val bars : Option[FlotBarsOptions] = None
  val color : Option [Either [String, Int]] = None
  val shadowSize: Option [Int] = None
}


