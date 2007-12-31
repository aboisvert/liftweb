package net.liftweb.util

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner

/*
class ToHeadTest extends JUnit3(ToHeadSpecs)
object ToHeadSpecsRunner extends ConsoleRunner(ToHeadSpecs)

object ToHeadSpecs extends Specification {
   "lift <head> merger" should {
     "merge /html/body//head into existing /html/head section" >> {
       val actual = <html>
         <head>
           <title>hello</title>
         </head>
         <body>
           blablabla
           <head>
             <script src="myScript.js"></script>
           </head>
           <div>
             sub section
             <head>
               <style>
               .myClass {{
                 text-align:right;
                 }}               
               </style>
             </head>
           </div>
         </body>
       </html>
       ;
       val expected = <html>
         <head>
           <title>hello</title>
           <script src="myScript.js"></script>
           <style><![CDATA[
           .myClass {
             text-align:right;
           }
           ]]></style>
         </head>
         <body>
           blablabla
           <div>
             sub section
           </div>
         </body>
       </html>
       HeadHelper.mergeToHtmlHead(actual) must equalIgnoreSpace(expected)
     }
     
     "merge <head> from real example" >> {
       val actual = <html xmlns:lift="http://liftweb.net/" xmlns="http://www.w3.org/1999/xhtml">
          <head>
            <meta content="text/html; charset=UTF-8" http-equiv="content-type"></meta>
            <meta content="" name="description"></meta>
            <meta content="" name="keywords"></meta>
          
            <title>lift webapptest</title>
            <script type="text/javascript" src="/scripts/jquery-1.2.1.js"></script>
          </head>
          <body>
            <head>
              <script src="foo.js" id="fromFrag"></script>
            </head>
            <h2>Welcome to the your project!</h2>
            <ul><li><a href="/">Home</a></li><li><a href="/htmlFragmentWithHead" id="current">htmlFragmentWithHead</a></li><li><a href="/htmlSnippetWithHead">htmlSnippetWithHead</a></li></ul>
          </body>
        </html>
       val expected = <html xmlns:lift="http://liftweb.net/" xmlns="http://www.w3.org/1999/xhtml">
          <head>
            <meta content="text/html; charset=UTF-8" http-equiv="content-type"></meta>
            <meta content="" name="description"></meta>
            <meta content="" name="keywords"></meta>
          
            <title>lift webapptest</title>
            <script type="text/javascript" src="/scripts/jquery-1.2.1.js"></script>
            <script src="foo.js" id="fromFrag"></script>
          </head>
          <body>
            <h2>Welcome to the your project!</h2>
            <ul><li><a href="/">Home</a></li><li><a href="/htmlFragmentWithHead" id="current">htmlFragmentWithHead</a></li><li><a href="/htmlSnippetWithHead">htmlSnippetWithHead</a></li></ul>
          </body>
        </html>
       HeadHelper.mergeToHtmlHead(actual) must equalIgnoreSpace(expected)
     }
     
     "merge <lift:tohead> into a new head if not previously exist" >> {
       val actual = <html>
         <body>
           blablabla
           <head>
             <script src="myScript.js"></script>
           </head>
           <div>
             sub section
             <head>
               <style>
               .myClass {{
                 text-align:right;
               }}
               </style>
             </head>
           </div>
         </body>
       </html>

       val expected = <html>
         <head>
           <script src="myScript.js"></script>
           <style>
           .myClass {{
             text-align:right;
           }}
           </style>
         </head>
         <body>
           blablabla
           <div>
             sub section
           </div>
         </body>
       </html>
       HeadHelper.mergeToHtmlHead(actual) must equalIgnoreSpace(expected)
     }
   }

   "lift head cleaner" should {
     "remove duplicate title tag" >> {
       val actual = <title>hello</title><title>hello2</title><title>hello3</title>
       val expected = <title>hello</title>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove script tag with same id as previous script tag" >> {
       val invariant = <script id="sc1" src="foo1.js"></script><script id="sc2" src="foo2.js"></script>
       HeadHelper.cleanHead(invariant) must equalIgnoreSpace(invariant)

       val actual = <script id="sc1" src="foo1.js"></script><script id="sc1" src="foo2.js"></script>
       val expected = <script id="sc1" src="foo1.js"></script>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove script tag with src attributes if src attributes are equals to previous script" >> {
       val actual = <script id="sc1" src="foo1.js"></script><script src="foo1.js"></script>
       val expected = <script id="sc1" src="foo1.js"></script>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)

       val actual2 = <script id="sc1" src="foo1.js"></script><script id="sc2" src="foo1.js"></script>
       val expected2 = <script id="sc1" src="foo1.js"></script>
       HeadHelper.cleanHead(actual2) must equalIgnoreSpace(expected2)
     }
     "remove script tag if content are equals to previous script (need to trim each line ?)" >> {
       val actual = <script>alert("hello");</script><script>alert("hello");</script>
       val expected = <script>alert("hello");</script>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove link to css with same id as previous link tag" >> {
       val invariant = <link id="css1" rel="stylesheet" type="text/css" href="style1.css"/><link id="css2" rel="stylesheet" type="text/css" href="style2.css"/>
       HeadHelper.cleanHead(invariant) must equalIgnoreSpace(invariant)

       val actual = <link id="css1" rel="stylesheet" type="text/css" href="style1.css"/><link id="css1" rel="stylesheet" type="text/css" href="style2.css"/>
       val expected = <link id="css1" rel="stylesheet" type="text/css" href="style1.css"/>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove link tag with href attributes if href attributes are equals to previous link" >> {
       val invariant = <link rel="stylesheet" type="text/css" href="style1.css"/><link rel="stylesheet" type="text/css" href="style2.css"/>
       HeadHelper.cleanHead(invariant) must equalIgnoreSpace(invariant)

       val actual = <link rel="stylesheet" type="text/css" href="style1.css"/><link rel="stylesheet" type="text/css" href="style1.css"/>
       val expected = <link rel="stylesheet" type="text/css" href="style1.css"/>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove style tag with same id as previous style tag" >> {
       val invariant = <style id="st1">.foo{{...}}</style><style id="st2">.bar{{...}}</style>
       HeadHelper.cleanHead(invariant) must equalIgnoreSpace(invariant)

       val actual = <style id="st1">.foo{{...}}</style><style id="st1">.bar{{...}}</style>
       val expected = <style id="st1">.foo{{...}}</style>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
     "remove style tag if content are equals to previous style (need to trim each line ?)" >> {
       val invariant = <style>.foo{{...}}</style><style>.bar{{...}}</style>
       HeadHelper.cleanHead(invariant) must equalIgnoreSpace(invariant)

       val actual = <style>.foo{{...}}</style><style>.foo{{...}}</style>
       val expected = <style>.foo{{...}}</style>
       HeadHelper.cleanHead(actual) must equalIgnoreSpace(expected)
     }
   }

} 
*/
