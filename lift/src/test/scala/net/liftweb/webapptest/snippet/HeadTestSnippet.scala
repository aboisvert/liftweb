package net.liftweb.webapptest.snippet

class HeadTestSnippet {
  def withHead = <div>
    <head>
      <script src="snippet.js"></script>
    </head>
    <span>Welcome to webtest1 at {new java.util.Date}</span>
  </div>
  
}

