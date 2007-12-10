import java.io._

def println(in: String) {scala.Console.println(in)}

def copy(src: File, dest: File) {
  src.listFiles.filter(f => f.isFile && !f.getName.startsWith(".")).
  foreach{
    f =>
      val df = new File(dest, f.getName)
    val fis = new BufferedInputStream(new FileInputStream(f))
    val fos = new BufferedOutputStream(new FileOutputStream(df))

    def cp {
      fis.read match {
	case -1 => fos.flush; fis.close; fos.close
	  case n => fos.write(n.toByte); cp
      }
    }
    
    cp
  }
}


if (args.length < 3) {
  println("usage: scala new_proj.scala project_name path package")
  println("eg: scala new_proj.scala Moose ~/scala/MooseProject com.mymoose")
  println("NB: Be sure to run new_proj.scala from the new_proj directory")
} else {
  val projName = args(0)
  val rootDir = new File(args(1))
  val pkg = args(2)
  val slashPack = pkg.replace(".", "/")

  // make main proj dir
  rootDir.mkdirs
  

  // make code directories
  var tmain = new File(rootDir, "src/main/scala")
  (new File(tmain, "bootstrap/liftweb")).mkdirs
  tmain = new File(tmain, slashPack)
  List("comet", "model", "snippet", "view").
  foreach(sd => (new File(tmain, sd)).mkdirs)

  // make web app directories
  tmain = new File(rootDir, "src/main/webapp")
  List("WEB-INF/classes", "WEB-INF/lib", "scripts", "style", "images",
     "templates-hidden").
  foreach(sd => (new File(tmain, sd)).mkdirs)

  val xDir = new File(new File(System.getProperty(("user.dir"))).
		       getParentFile, "sites/example")
  List("scripts", "style", "images").
  foreach(sd => copy(new File(xDir, "src/main/webapp/"+sd),
		     new File(tmain, sd)))

val pom = """<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
 <modelVersion>4.0.0</modelVersion>

 <groupId>"""+pkg+"""</groupId>
 <artifactId>"""+projName+"""</artifactId>
 <packaging>war</packaging>
 <name>"""+projName+"""</name>
 <version>0.1.0</version>
 <pluginRepositories>
  <pluginRepository>
   <id>scala-tools.org</id>
   <name>Scala Maven2 Repository</name>
      <url>http://scala-tools.org/repo-releases</url>
  </pluginRepository>
 </pluginRepositories>

  <dependencies>
    <dependency>
      <groupId>javax.servlet</groupId>
      <artifactId>servlet-api</artifactId>
      <version>2.4</version>
      <scope>provided</scope>
    </dependency>
    <dependency>
      <groupId>net.liftweb</groupId>
      <artifactId>lift-core</artifactId>
      <version>0.4-SNAPSHOT</version>
    </dependency>
    <dependency>
      <groupId>scala</groupId>
      <artifactId>scala-library</artifactId>
      <version>2.6.0</version>
    </dependency>
    <dependency>
      <groupId>org.apache.derby</groupId>
      <artifactId>derby</artifactId>
      <version>10.2.2.0</version>
    </dependency>
    <dependency>
      <groupId>javax.mail</groupId>
      <artifactId>mail</artifactId>
      <version>1.4</version>
    </dependency>
    <dependency>
      <groupId>javax.activation</groupId>
      <artifactId>activation</artifactId>
      <version>1.1</version>
    </dependency>
    <dependency>
      <groupId>mysql</groupId>
      <artifactId>mysql-connector-java</artifactId>
      <version>5.0.4</version>
    </dependency>
    <dependency>
      <groupId>com.rabbitmq</groupId>
      <artifactId>rabbitmq-client</artifactId>
      <version> 1.2.0</version>
    </dependency>
  </dependencies>

  <build>
    <sourceDirectory>src/main/scala</sourceDirectory>
    <plugins>
      <plugin>
        <groupId>org.scala-tools</groupId>
        <artifactId>maven-scala-plugin</artifactId>
        <version>2.0</version>
        <executions>
          <execution>
            <goals>
              <goal>compile</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>org.mortbay.jetty</groupId>
        <artifactId>maven-jetty-plugin</artifactId>
        <version>6.1.3</version>
        <configuration>
          <systemProperties>
            <systemProperty>
              <name>org.apache.cocoon.log4j.loglevel</name>
              <value>WARN</value>
            </systemProperty>
          </systemProperties>
          <connectors>
            <connector implementation="org.mortbay.jetty.nio.SelectChannelConnector">
              <port>8888</port>
              <maxIdleTime>30000</maxIdleTime>
            </connector>
          </connectors>
          <contextPath>/</contextPath>
          <scanIntervalSeconds>5</scanIntervalSeconds>
        </configuration>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-eclipse-plugin</artifactId>
        <configuration>
          <downloadSources>true</downloadSources>
          <buildcommands>
            <buildcommand>ch.epfl.lamp.sdt.core.scalabuilder</buildcommand>
          </buildcommands>
          <additionalProjectnatures>
            <projectnature>ch.epfl.lamp.sdt.core.scalanature</projectnature>
          </additionalProjectnatures>
          <classpathContainers>
            <classpathContainer>org.eclipse.jdt.launching.JRE_CONTAINER</classpathContainer>
            <classpathContainer>ch.epfl.lamp.sdt.launching.SCALA_CONTAINER</classpathContainer>
          </classpathContainers>
        </configuration>
      </plugin>
    </plugins>
  </build>

  <reporting>
    <plugins>
      <plugin>
        <groupId>org.scala-tools</groupId>
        <artifactId>maven-scala-plugin</artifactId>
      </plugin>
    </plugins>
  </reporting>

</project>"""
  var pw = new PrintWriter(new FileWriter(new File(rootDir, "pom.xml")))
  pw.println(pom)
  pw.flush
  pw.close

  val snippet = "package "+pkg+""".snippet

class HelloWorld {
  def howdy = <span>Welcome to """+projName+""" at {new java.util.Date}</span>
}
"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, "src/main/scala/"+slashPack+"/snippet/HelloWorld.scala")))
  pw.println(snippet)
  pw.flush
  pw.close

  val webxml = """<?xml version="1.0" encoding="ISO-8859-1"?>

<!DOCTYPE web-app
PUBLIC "-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN"
"http://java.sun.com/j2ee/dtds/web-app_2_3.dtd">

<web-app>
<filter>
  <filter-name>LiftFilter</filter-name>
  <display-name>Lift Filter</display-name>
  <description>The Filter that intercepts lift calls</description>
  <filter-class>net.liftweb.http.LiftFilter</filter-class>
</filter>
  	

<filter-mapping>
  <filter-name>LiftFilter</filter-name>
  <url-pattern>/*</url-pattern>
</filter-mapping>

</web-app>
"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, "src/main/webapp/WEB-INF/web.xml")))
  pw.println(webxml)
  pw.flush
  pw.close

val boot = """package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import """+pkg+""".model._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    LiftServlet.addToPackages("""+"\""+pkg+"\""+""")
    Schemifier.schemify(true, Log.infoF _, User)
    LiftServlet.addTemplateBefore(User.templates)

    val entries = Menu(Loc("Home", "/", "Home")) :: User.sitemap
    LiftServlet.setSiteMap(SiteMap(entries:_*))
    S.addAround(User.requestLoans)
  }
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm = DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}
"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, "src/main/scala/bootstrap/liftweb/Boot.scala")))
  pw.println(boot)
  pw.flush
  pw.close

  val user = """package """+pkg+""".model

import net.liftweb.mapper._
import net.liftweb.util._

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User, User with KeyedMetaMapper[Long, User]] {
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
			       <lift:bind /></lift:surround>)
  // define the order fields will appear in forms and output
  override def fieldOrder = id :: firstName :: lastName :: email :: 
  locale :: timezone ::
  password :: textArea :: Nil
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  
  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}
"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, "src/main/scala/"+
  slashPack+"/model/User.scala")))
  pw.println(user)
  pw.flush
  pw.close

val index = """<lift:surround with="default" at="content">
		<h2>Welcome to the your project!</h2>
		<p><lift:snippet type="hello_world:howdy" /></p>
  <p>You can type 'mvn eclipse:eclipse' in your project directory
     to create an <a href='http://eclipse.org'>Eclipse</a> project
     for this code.</p>
</lift:surround>
"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, 
  "src/main/webapp/index.html")))
  pw.println(index)
  pw.flush
  pw.close

val template = """<html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift="http://liftweb.net/">
<head>
	<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
	<meta name="description" content="" />
	<meta name="keywords" content="" />

	<title>Lift Web Framework</title>
	<link rel="stylesheet" href="/style/style.css" media="screen" />
	<script src="/scripts/jquery-cur.min.js" type="text/javascript"/>
</head>
<body>
	<div id="container">
		<div id="header">
			<h1>"""+projName+"""</h1>
			<div style='padding: 0px 10px 10px 10px'>Your application</div>
		</div>
		<div id="content">
		<lift:bind name="content" />
		</div>
		<div id="sidebar">
                  <lift:snippet type="Menu:builder" />	
		  <div><lift:snippet type="error_report"/></div>
		</div>
		<div id="footer"><a href='http://liftweb.net'><i>lift</i></a> is Copyright 2007 WorldWide Conferencing, LLC.  Distributed under an Apache 2.0 License.
         </div>
	</div>
	
</body>
</html>"""

  pw = new PrintWriter(new FileWriter(new File(rootDir, "src/main/webapp/templates-hidden/default.html")))
  pw.println(template)
  pw.flush
  pw.close

  (new File(rootDir, "script")).mkdirs
  

  val liftrepl = """import bootstrap.liftweb.Boot
import scala.tools.nsc.MainGenericRunner

// Instantiate your project's Boot file
val b = new Boot(); 
// Boot your project
b.boot;
// Now run the MainGenericRunner to get your repl
MainGenericRunner.main(args)
// After the repl exits, then exit the scala script
exit(0)
"""
  pw = new PrintWriter(new FileWriter(new File(rootDir, "script/liftrepl.scala")))
  pw.println(liftrepl)
  pw.flush
  pw.close

  val console = """#!/bin/sh
# Run from the root of your project directory (where your pom.xml file resides)
# It will give you a scala interpreter with all of your project's classfiles 
# loaded and ready to go.
#
# Author: Steve Jenson (stevej@pobox.com) Nov 1, 2007
#

THE_CLASSPATH=
for i in `find ./ -name "*.jar" -print`
do
  THE_CLASSPATH=${THE_CLASSPATH}:${i}
done

for i in `find ~/.m2 -name "*.jar" -print`
do
  THE_CLASSPATH=${THE_CLASSPATH}:${i}
done

CLASSPATH=$THE_CLASSPATH:target/classes scala ./script/liftrepl.scala
"""
  pw = new PrintWriter(new FileWriter(new File(rootDir, "script/console")))
  pw.println(console)
  pw.flush
  pw.close

  println(projName+" created at "+rootDir)
  println("Run cd "+rootDir+"; mvn install jetty:run")
  println("then point your brower to http://localhost:8888")
  println("to use script/console, you must chmod +x it first")
}

