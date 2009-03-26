require 'buildr/scala'

repositories.remote = [ "http://mirrors.ibiblio.org/pub/mirrors/maven2/", 
                        "http://scala-tools.org/repo-releases/",
                        "http://ftp.cica.es/mirrors/maven2/" ]

SLF4J_VERSION = "1.5.6"

# Compile dependencies
CODEC = "commons-codec:commons-codec:jar:1.3"
COLLECTIONS = "commons-collections:commons-collections:jar:3.2.1"
COMMONS_LOGGING = "commons-logging:commons-logging:jar:1.0.4"

FILEUPLOAD  = "commons-fileupload:commons-fileupload:jar:1.2.1"
HTTPCLIENT  = "commons-httpclient:commons-httpclient:jar:3.1"
JAVAMAIL = "javax.mail:mail:jar:1.4"
LOG4J = "log4j:log4j:jar:1.2.14"
SLF4J = "org.slf4j:slf4j-api:jar:#{SLF4J_VERSION}"
ACTIVATION = "javax.activation:activation:jar:1.1"
SERVLET_API = "javax.servlet:servlet-api:jar:2.5"
SMACK = [ "org.igniterealtime.smack:smack:jar:3.1.0",
          "org.igniterealtime.smack:smackx:jar:3.1.0" ]
OPENID = "org.openid4java:openid4java:jar:0.9.3"
RABBITMQ = "com.rabbitmq:rabbitmq-client:jar:1.3.0"          

# Testing dependencies
JETTY = ["org.mortbay.jetty:jetty:jar:6.1.6", 
         "org.mortbay.jetty:jetty-util:jar:6.1.6",
         "regexp:regexp:jar:1.3"]
SLF4J_SIMPLE = "org.slf4j:slf4j-simple:jar:#{SLF4J_VERSION}"
JWEBUNIT = [ "net.sourceforge.jwebunit:jwebunit-core:jar:1.4.1",
             "net.sourceforge.jwebunit:jwebunit-htmlunit-plugin:jar:1.4.1" ]
JUNIT = "junit:junit:jar:4.5"

# Optional dependencies
DERBY = "org.apache.derby:derby:jar:10.4.2.0"
H2_DATABASE = "com.h2database:h2:jar:1.0.79"
POSTGRESQL = "postgresql:postgresql:jar:8.3.603.jdbc3"
MYSQL = "mysql:mysql-connector-java:jar:5.1.6"

COMPILE = [CODEC, FILEUPLOAD, JAVAMAIL, SERVLET_API]

define "liftweb" do
  project.version = "1.1-SNAPSHOT"
  project.group = "net.liftweb"

  define "lift" do
    compile.with projects("lift-util"), COMPILE
    test.using :specs
    test.using :properties => {"net.liftweb.webapptest.src.test.webapp" => _("src/test/webapp")}
    test.with JETTY, JWEBUNIT
    package :jar
  end

  define "lift-mapper" do
    compile.with projects("lift", "lift-util"), COMPILE
    test.using :specs
    package :jar
  end

  define "lift-archetype-basic" do
    package :jar
  end

  define "lift-widgets" do
    compile.with projects("lift", "lift-util"), COMPILE
    package :jar
  end

  define "lift-facebook" do
    compile.with projects("lift", "lift-util"), COMPILE
    package :jar
  end

  define "lift-textile" do
    compile.with projects("lift", "lift-util"), COMPILE
    test.using :specs
    package :jar
  end

  define "archetype-jpa-basic" do
    compile.with projects("lift", "lift-util"), COMPILE
  end

  define "lift-xmpp" do
    compile.with projects("lift", "lift-util"), COMPILE, SMACK
    package :jar
  end

  define "lift-paypal" do
    compile.with projects("lift", "lift-util"), COMPILE, HTTPCLIENT
    package :jar
  end

  define "lift-testkit" do
    compile.with projects("lift", "lift-util"), COMPILE, COMMONS_LOGGING, HTTPCLIENT
    package :jar
  end

  define "lift-archetype-blank" do
    compile.with projects("lift", "lift-util"), COMPILE
    package :jar
  end

  define "lift-openid" do
    compile.with projects("lift", "lift-mapper", "lift-util"), COMPILE, OPENID
    package :jar
  end

  define "lift-oauth" do
    compile.with projects("lift", "lift-util"), COMPILE
    package :jar
  end

  define "lift-machine" do
    compile.with projects("lift", "lift-mapper", "lift-util"), COMPILE
    package :jar
  end

  define "lift-amqp" do
    compile.with projects("lift", "lift-util"), COMPILE, RABBITMQ
    package :jar
  end

  define "lift-util" do
    compile.with ACTIVATION, CODEC, COLLECTIONS, JAVAMAIL, LOG4J, SLF4J
    test.using :specs
    test.compile.from _("src/test/scala")
    package :jar
  end

  define "lift-record" do
    compile.with projects("lift", "lift-mapper", "lift-util")
    package :jar
  end

end

=begin
./lift/src/main/scala
./sites/skittr/src/main/scala
./sites/hellolift/src/main/scala
./sites/example/src/main/scala
./sites/flotDemo/src/main/scala
./sites/http-authentication/src/main/scala
./sites/JPADemo/JPADemo-spa/src/main/scala
./sites/JPADemo/JPADemo-web/src/main/scala
./sites/hellodarwin/src/main/scala
=end