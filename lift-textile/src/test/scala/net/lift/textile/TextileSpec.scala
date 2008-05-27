/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.textile

import org.specs._
import org.specs.runner._
import org.specs.Sugar._

import scala.xml._

class TextileSpecTest extends Runner(TextileSpec) with JUnit with Console
object TextileSpec extends Specification {
  import TextileParser._
  
  "A Textile Parse" can {
    "Be a single line of text" in {
      toHtml("Hello World", None) must ==/(<p>Hello World</p>)
    }
    
    "Make things bold" in {
      toHtml("**Hello World**", None) must ==/(<p><b>Hello World</b></p>)
    }
    
    "I am <em>very</em> serious" in {
      toHtml("I am <em>very</em> serious", None) must ==/(<p>I am <em>very</em> serious</p>)
    }
    
    "Observe -- very nice!" in {
      toHtml("Observe -- very nice!", None) must ==/(<p>Observe &#8212; very nice!</p>)
    }
    
    "Observe - tiny and brief." in {
      toHtml("Observe - tiny and brief.", None) must ==/(<p>Observe &#8211; tiny and brief.</p>)
    }
    
    "\"Observe!\"" in {
      toHtml("\"Observe!\"", None) must ==/(<p>&#8220;Observe!&#8221;</p>)
    }
  }
}
