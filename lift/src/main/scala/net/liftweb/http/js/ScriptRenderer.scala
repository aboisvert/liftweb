/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
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
package net.liftweb.http.js

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

object ScriptRenderer {

  /**
   * Renders the default ajax script use by lift
   */
  def ajaxScript = JsCmds.Run("""
var lift_ajaxQueue = [];
var lift_ajaxInProcess = null;
var lift_ajaxShowing = false;
var lift_ajaxRetryCount = """+
                        (LiftRules.ajaxRetryCount openOr 3)+
                        """

function lift_ajaxHandler(theData, theSuccess, theFailure) {
  var toSend = {retryCnt: 0};
  toSend.when = (new Date()).getTime();
  toSend.theData = theData;
  toSend.onSuccess = theSuccess;
  toSend.onFailure = theFailure;

  lift_ajaxQueue.push(toSend);
  lift_ajaxQueueSort();
  lift_doAjaxCycle();
  return false; // buttons in forms don't trigger the form
}

function lift_ajaxQueueSort() {
  lift_ajaxQueue.sort(function (a,b) {return a.when - b.when;});
}

function lift_defaultFailure() {
"""+
                        (LiftRules.ajaxDefaultFailure.map(_().toJsCmd) openOr "")+
                        """
}

function lift_startAjax() {
  lift_ajaxShowing = true;
"""+
                        (LiftRules.ajaxStart.map(_().toJsCmd) openOr "")+
                        """
}

function lift_endAjax() {
  lift_ajaxShowing = false;
"""+
                        (LiftRules.ajaxEnd.map(_().toJsCmd) openOr "")+
                        """
}

function lift_testAndShowAjax() {
  if (lift_ajaxShowing && lift_ajaxQueue.length == 0 &&
      lift_ajaxInProcess == null) {
   lift_endAjax();
      } else if (!lift_ajaxShowing && (lift_ajaxQueue.length > 0 ||
     lift_ajaxInProcess != null)) {
   lift_startAjax();
     }
}

function lift_traverseAndCall(node, func) {
  if (node.nodeType == 1) func(node);
  var i = 0;
  var cn = node.childNodes;
  
  for (i = 0; i < cn.length; i++) {
    lift_traverseAndCall(cn.item(i), func);
  }
}

function lift_findGCNodes() {
  var ret = [];
  lift_traverseAndCall(document, function(e) {
    if (e.attributes['lift:gc']) {
      ret.push(e.attributes['lift:gc'].value);
    }
  });
  return ret;
}

function lift_successRegisterGC() {
  setTimeout("lift_registerGC()", 75000);
}

function lift_failRegisterGC() {
  setTimeout("lift_registerGC()", 15000);
}

function lift_registerGC() {
  var nodes = lift_findGCNodes();
  var data = "__lift__GCNodes="+encodeURIComponent("""+
   LiftRules.jsArtifacts.jsonStringify(JE.JsRaw("nodes")).toJsCmd+
  """);
""" +
                        LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
                                                            "POST",
                                                            LiftRules.ajaxPostTimeout,
                                                            false, "script",
                                                            Full("lift_successRegisterGC"), Full("lift_failRegisterGC")))+
                        """  
}

function lift_doAjaxCycle() {
  var queue = lift_ajaxQueue;
  if (queue.length > 0) {
    var now = (new Date()).getTime();
    if (lift_ajaxInProcess == null && queue[0].when <= now) {
      var aboutToSend = queue.shift();

      lift_ajaxInProcess = aboutToSend;
      var  successFunc = function() {
         lift_ajaxInProcess = null;
         if (aboutToSend.onSuccess) {
           aboutToSend.onSuccess();
         }
         lift_doAjaxCycle();
      };

      var failureFunc = function() {
         lift_ajaxInProcess = null;
         var cnt = aboutToSend.retryCnt;
         if (cnt < lift_ajaxRetryCount) {
	   aboutToSend.retryCnt = cnt + 1;
           var now = (new Date()).getTime();
           aboutToSend.when = now + (1000 * Math.pow(2, cnt));
           queue.push(aboutToSend);
           lift_ajaxQueueSort();
         } else {
           if (aboutToSend.onFailure) {
             aboutToSend.onFailure();
           } else {
             lift_defaultFailure();
           }
         }
         lift_doAjaxCycle();
      };
      lift_actualAjaxCall(aboutToSend.theData, successFunc, failureFunc);
    }
  }

  lift_testAndShowAjax();
  setTimeout("lift_doAjaxCycle();", 200);
}

function lift_blurIfReturn(e) {
  var code;
	if (!e) var e = window.event;
	if (e.keyCode) code = e.keyCode;
	else if (e.which) code = e.which;

  var targ;

	if (e.target) targ = e.target;
	else if (e.srcElement) targ = e.srcElement;
	if (targ.nodeType == 3) // defeat Safari bug
		targ = targ.parentNode;

  if (code == 13) {targ.blur(); return false;} else {return true;};
}

function lift_actualAjaxCall(data, onSuccess, onFailure) {
""" +
                        LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
                                                            "POST",
                                                            LiftRules.ajaxPostTimeout,
                                                            false, "script",
                                                            Full("onSuccess"), Full("onFailure")))+
                        """
}

""" +
                        LiftRules.jsArtifacts.onLoad(new JsCmd() {def toJsCmd = "lift_doAjaxCycle()"}).toJsCmd)


  /**
   * Renders the default JS comet script
   */
  def cometScript = JsCmds.Run("""
      function lift_handlerSuccessFunc() {setTimeout("lift_cometEntry();",100);}
      function lift_handlerFailureFunc() {setTimeout("lift_cometEntry();",10000);}
      function lift_cometEntry() {""" +
                        LiftRules.jsArtifacts.comet(AjaxInfo(JE.JsRaw("lift_toWatch"),
                                                             "GET",
                                                             LiftRules.cometGetTimeout,
                                                             false,
                                                             "script",
                                                             Full("lift_handlerSuccessFunc"),
                                                             Full("lift_handlerFailureFunc"))) + " } \n" +
                        LiftRules.jsArtifacts.onLoad(new JsCmd() {
        def toJsCmd = "lift_handlerSuccessFunc()"
      }).toJsCmd)
}
