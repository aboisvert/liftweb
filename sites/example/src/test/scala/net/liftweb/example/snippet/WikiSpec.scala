package net.liftweb.example.snippet
import _root_.org.specs._
import _root_.org.specs.Sugar._
import _root_.org.specs.runner._
import _root_.net.liftweb.example.model._
import _root_.net.liftweb.http.{S, Req, LiftSession}
import _root_.net.liftweb.util.{Full, Empty}

// This file crashes the compiler. TODO: Investigate, report, and fix

// class WikiTest extends JUnit4(WikiSpec)
// object WikiSpec extends Specification with MockEntries {
//   "In the following spec, 'pageName' refers to the value of the S parameter 'wiki_page'" +
//   "The 'main' function" should { doBefore { createMocks }
//     "return all existing entries if pageName is 'all'" in {
//       withEntries(WikiEntry.create.name("EntryOne"), WikiEntry.create.name("EntryTwo"))
//       userRequests("all")
//       inSession {
//         wikiMain must \\("a", Map("href" -> "/wiki/EntryOne"))
//         wikiMain must \\("a", Map("href" -> "/wiki/EntryTwo"))
//       }
//     }
//     "return a new page with a form for a 'HomePage' entry if the wiki_page parameter is not specified" in {
//       withNoEntries; userRequests("nothing")
//       inSession {
//         wikiMain must \\("form", Map("action" -> "/wiki/HomePage", "method" -> "GET"))
//       }
//     }
//     "return a new page with a form for a 'NewEntry' entry if there is no entry with the name 'NewEntry' in the database" in {
//       withNoEntries; userRequests("NewEntry")
//       inSession {
//         wikiMain must \\("form", Map("action" -> "/wiki/NewEntry", "method" -> "GET"))
//       }
//     }
//     "return an existing entry if there is an entry named 'ExistingEntry' in the database" in {
//       withEntries(WikiEntry.create.name("ExistingEntry")); userRequests("ExistingEntry")
//       inSession {
//         wikiMain must \\("form", Map("action" -> "/wiki/ExistingEntry", "method" -> "GET"))
//       }
//     }
//   }
//   "A newly created entry" should { doBefore { createMocks }
//     "be named 'HomePage' if pageName is not specified" in {
//       withNoEntries; userRequests("nothing")
//       inSession {
//         wikiMain.toString must include("Create Entry named HomePage")
//       }
//     }
//     "be named 'pageName' if pageName is specified" in {
//       withNoEntries; userRequests("MyPage")
//       inSession {
//         wikiMain.toString must include("Create Entry named MyPage")
//       }
//     }
//   }
// }
// 
// import _root_.net.liftweb.mapper._
// import _root_.net.liftweb.example.model._
// import _root_.net.liftweb.example.snippet._
// trait MockEntries extends MockRequest {
//   var wikiEntries: MetaWikiEntry = _
//   var requested = "all"
//   def wikiMain = {
//     trait MockedMetaWikiEntry extends MetaWikiEntry {
//       override def find(q: QueryParam[WikiEntry]*) = wikiEntries.find(q:_*)
//       override def findAll(q: QueryParam[WikiEntry]*) = wikiEntries.findAll(q:_*)
//       override def create = wikiEntries.create
//       override def findAll = wikiEntries.findAll
//     }
//     val wiki = new Wiki with MockedMetaWikiEntry
//     wiki.main
//   }
//   override def createMocks = {
//     super.createMocks
//     wikiEntries = mock[MetaWikiEntry]
//   }
//   def userRequests(page: String) {
//     if (page == "nothing")
//       unsetParameter("wiki_page")
//     else
//       setParameter("wiki_page", page)
//     requested = page
//   }
//   def withNoEntries = {
//     expect {
//       0.atLeastOf(wikiEntries).find(any(classOf[QueryParam[WikiEntry]])).willReturn(Empty)
//       0.atLeastOf(wikiEntries).create.willReturn(new WikiEntry)
//     }
//   }
//   def withEntries(entries: WikiEntry*) = {
//     expect {
//       if (entries.isEmpty)
//         one(wikiEntries).find(any(classOf[QueryParam[WikiEntry]])).willReturn(Empty)
//       else if (requested == "all")
//         0.atLeastOf(wikiEntries).findAll willReturn entries.toList
//       else
//         one(wikiEntries).find(any(classOf[QueryParam[WikiEntry]])).willReturn(Full(entries(0)))
//       0.atLeastOf(wikiEntries).findAll(any(classOf[QueryParam[WikiEntry]])).willReturn(entries.toList)
//     }
//   }
// }
// import _root_.org.specs.mock._
// import _root_.javax.servlet.http._
// trait MockRequest extends JMocker with ClassMocker {
//   var request = mock[Req]
//   var httpRequest = mock[HttpServletRequest]
//   var session = mock[LiftSession]
//   def createMocks = {
//     request = mock[Req]
//     httpRequest = mock[HttpServletRequest]
//     session = mock[LiftSession]
//     expect {
//       0.atLeastOf(request).request.willReturn(httpRequest)
//       0.atLeastOf(httpRequest).getCookies
//     }
//   }
//   def inSession(f: => Any) {
//     S.init(request, session) {
//       f
//     }
//   }
//   def unsetParameter(name: String) {
//     expect {
//       0.atLeastOf(request).param(equal(name)).willReturn(None)
//     }
//   }
//   def setParameter(name: String, value: String) {
//     expect {
//       0.atLeastOf(request).param(equal(name)).willReturn(Some(value))
//     }
//   }
// }