package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.net.InetAddress
import java.util.Properties
import Helpers._
import org.apache.log4j._
import org.apache.log4j.xml._

/**
 * A thin wrapper around log4j
 */
object Log {
  val defaultProps = 
    """<?xml version="1.0" encoding="UTF-8" ?>
    <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
  <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">
  <appender name="appender" class="org.apache.log4j.ConsoleAppender">
  <layout class="org.apache.log4j.SimpleLayout"/>
  </appender>
  <root>
  <priority value ="INFO"/>
  <appender-ref ref="appender"/>
  </root>
  </log4j:configuration>
"""
  
  val log4jUrl = first(Props.toTry){f => println("Log trying "+f()) ; tryo(getClass.getResource(f()+"log4j.props")) match {case None => None; case Some(s) if s eq null => None; case s =>println("Success "+f()); s}}
  
  log4jUrl.foreach(url => PropertyConfigurator.configure(url))
  if (!log4jUrl.isDefined) {
    val domConf = new DOMConfigurator
    val defPropBytes = defaultProps.toString.getBytes("UTF-8")
    val is = new java.io.ByteArrayInputStream(defPropBytes)
    domConf.doConfigure(is, LogManager.getLoggerRepository())
  }
  def logger(clz: Class): LiftLogger = new LiftLogger(Logger.getLogger(clz))
  def logger(name: String): LiftLogger = new LiftLogger(Logger.getLogger(name))
  val rootLogger: LiftLogger = logger("lift") // new LiftLogger(Logger.getRootLogger)

  def trace(msg: => AnyRef) = rootLogger.trace(msg)
   def trace(msg: => AnyRef, t: => Throwable) = rootLogger.trace(msg, t)
   
   def assertLog(assertion: Boolean, msg: => String) = rootLogger.assertLog(assertion, msg)
   
   def isDebugEnabled = rootLogger.isDebugEnabled
   def debug(msg: => AnyRef) = rootLogger.debug(msg)
   def debug(msg: => AnyRef, t: => Throwable) = rootLogger.debug(msg, t)
   
   def isErrorEnabled = rootLogger.isEnabledFor(Level.ERROR)
   def error(msg: => AnyRef) = rootLogger.error(msg)
   def error(msg: => AnyRef, t: => Throwable) = rootLogger.error(msg, t)

   def fatal(msg: AnyRef) = rootLogger.fatal(msg)
   def fatal(msg: AnyRef, t: Throwable) = rootLogger.fatal(msg, t)
   
   def level = rootLogger.level
   def level_=(level: Level) = rootLogger.level = level
   def name = rootLogger.name
   def parent = rootLogger.parent
   
   def isInfoEnabled = rootLogger.isInfoEnabled
   def info(msg: => AnyRef) = rootLogger.info(msg)
   def info(msg: => AnyRef, t: => Throwable) = rootLogger.info(msg, t)


   def isEnabledFor(level: Priority) = rootLogger.isEnabledFor(level)
   
   def isWarnEnabled = rootLogger.isWarnEnabled
   def warn(msg: => AnyRef) = rootLogger.warn(msg)
   def warn(msg: => AnyRef, t: => Throwable) = rootLogger.warn(msg, t)  
}

class LiftLogger(val logger: Logger) {
   def isTraceEnabled = logger.isTraceEnabled
   def trace(msg: => AnyRef) = if (isTraceEnabled) logger.trace(msg)
   def trace(msg: => AnyRef, t: => Throwable) = if (isTraceEnabled) logger.trace(msg, t)
   
   def assertLog(assertion: Boolean, msg: => String) = if (assertion) logger.assertLog(assertion, msg)
   
   def isDebugEnabled = logger.isDebugEnabled
   def debug(msg: => AnyRef) = if (isDebugEnabled) logger.debug(msg)
   def debug(msg: => AnyRef, t: => Throwable) = if (isDebugEnabled) logger.debug(msg, t)
   
   def isErrorEnabled = logger.isEnabledFor(Level.ERROR)
   def error(msg: => AnyRef) = if (isErrorEnabled) logger.error(msg)
   def error(msg: => AnyRef, t: => Throwable) = if (isErrorEnabled) logger.error(msg, t)

   def fatal(msg: AnyRef) = logger.fatal(msg)
   def fatal(msg: AnyRef, t: Throwable) = logger.fatal(msg, t)
   
   def level = logger.getLevel
   def level_=(level: Level) = logger.setLevel(level)
   def name = logger.getName
   def parent = logger.getParent
   
   def isInfoEnabled = logger.isInfoEnabled
   def info(msg: => AnyRef) = if (isInfoEnabled) logger.info(msg)
   def info(msg: => AnyRef, t: => Throwable) = if (isInfoEnabled) logger.info(msg, t)


   def isEnabledFor(level: Priority) = logger.isEnabledFor(level)
   
   def isWarnEnabled = isEnabledFor(Level.WARN)
   def warn(msg: => AnyRef) = if (isWarnEnabled) logger.warn(msg)
   def warn(msg: => AnyRef, t: => Throwable) = if (isWarnEnabled) logger.warn(msg, t)
}
