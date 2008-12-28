package net.liftweb.util

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

import _root_.java.net.InetAddress
import _root_.java.util.Properties
import Helpers._

/**
 * Property management
 */
object Props {
  /**
   * Get the property value
   * @param name the property to get
   *
   * @return the value of the property if defined
   */
  def get(name: String): Box[String] = Box(props.get(name))

  // def apply(name: String): String = props(name)

  def getInt(name: String): Box[Int] = get(name).map(toInt) // toInt(props.get(name))
  def getInt(name: String, defVal: Int): Int = getInt(name) openOr defVal // props.get(name).map(toInt(_)) getOrElse defVal
  def getLong(name: String): Box[Long] = props.get(name).flatMap(asLong)
  def getLong(name: String, defVal: Long): Long = getLong(name) openOr defVal // props.get(name).map(toLong(_)) getOrElse defVal
  def getBool(name: String): Box[Boolean] = props.get(name).map(toBoolean) // (props.get(name))
  def getBool(name: String, defVal: Boolean): Boolean = getBool(name) openOr defVal // props.get(name).map(toBoolean(_)) getOrElse defVal
  def get(name: String, defVal: String) = props.get(name) getOrElse defVal

  def require(what: String*) = what.filter(!props.contains(_))

  def requireOrDie(what: String*) {
    require(what :_*).toList match {
      case Nil =>
      case bad => throw new Exception("The following required properties are not defined: "+bad.mkString(","))
      }
   }

  object RunModes extends Enumeration {
    val Development = Value(1, "Development")
    val Test = Value(2, "Test")
    val Staging = Value(3, "Staging")
    val Production = Value(4, "Production")
    val Pilot = Value(5, "Pilot")
    val Profile = Value(6, "Profile")
  }

  import RunModes._

  val propFileName = "lift.props"
  val fileName = "lift.props"

lazy val mode = Box.legacyNullTest((System.getProperty("run.mode"))).map(_.toLowerCase) match {
    case Full("test") => Test
    case Full("production") => Production
    case Full("staging") => Staging
    case Full("pilot") => Pilot
    case Full("profile") => Profile
    case _ => Development
  }

  lazy val modeName = mode match {
    case Test => "test."
    case Staging => "staging."
    case Production => "production."
    case Pilot => "pilot."
    case Profile => "profile."
    case _ => ""
  }
  val userName = System.getProperty("user.name") +"."

  val hostName = InetAddress.getLocalHost.getHostName + "."

  val toTry: List[() => String] = List(() => "/props/" + modeName + userName + hostName,
					       () => "/props/" + modeName + userName,
					       () => "/props/" + modeName + hostName,
                                               () => "/props/"+ modeName + "default.",
					       () => "/" + modeName + userName + hostName,
					       () => "/" + modeName + userName,
					       () => "/" + modeName + hostName,
                                               () => "/"+ modeName +"default.")
  val props = {
    // find the first property file that is available
    first(toTry)(f => tryo(getClass.getResourceAsStream(f()+"props")).filter(_ ne null)).map{s => val ret = new Properties; ret.load(s); ret} match {

      // if we've got a propety file, create name/value pairs and turn them into a Map
      case Full(prop) =>
        Map(prop.entrySet.toArray.map{
          s2 =>
            val s = s2.asInstanceOf[_root_.java.util.Map.Entry[String, String]]
          (s.getKey,s.getValue)
        } :_*)

      case _ => Map.empty[String, String] // if none, it's an empty map
    }
  }
}
