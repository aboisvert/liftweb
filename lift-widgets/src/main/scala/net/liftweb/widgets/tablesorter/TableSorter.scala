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