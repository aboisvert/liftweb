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

package net.liftweb.widgets.tablesorter

import _root_.scala.xml.NodeSeq

class TableSorter {
  def init() {
    import _root_.net.liftweb.http.ResourceServer

    ResourceServer.allow({
      case "tablesorter" :: tail => true
    })
    println("register")
  }
  def render(selector: String) = {
    val onLoad ="""jQuery(function($){
            // tablesorter call - maybe here?
            $('"""+selector+"""').tablesorter({sortList:[[0,0]], widgets:['zebra']});
            });
            """
    <xml:group>
      <head>
        <link rel="stylesheet" href="/classpath/tablesorter/themes/blue/style.css" type="text/css" id="" media="print, projection, screen" />
        <script type="text/javascript" src="/classpath/tablesorter/jquery.tablesorter.js"></script>
        <script type="text/javascript" charset="utf-8">{onLoad}</script>
      </head>
      <table>
      <tr><td>33</td></tr>
      </table>
    </xml:group>
  }
}