package net.liftweb.flot_demo.web.model

import scala.collection.mutable.{HashMap, HashSet}

import scala.actors.Actor
import scala.actors.Actor._

import net.liftweb.widgets.flot._

/**
 * TO-DO: english translation
 */

case class Muestra (time : Long, medidas : List [Double]) {
  override def toString () = {
    "time: " + time +
      ", valores: " + medidas.foldLeft ("") ((sz, valor) => sz + " " + valor)
  }
}

//

case class AddListener(listener: Actor)

case class RemoveListener(listener: Actor)

/**
 * mantiene en memoria "max" muestras
 */

class AcumMuestrasActor (max : Int) extends Actor {

  val options = new FlotOptions () {
        override val xaxis = Some (new FlotAxisOptions () {
          override val mode = Some ("time")
        })
      }

  var series : List [FlotSerie] =
    new FlotSerie () {
      override val label = Some ("Serie 1")
      override val data = Nil
    } ::
    new FlotSerie () {
      override val label = Some ("Serie 2")
      override val data = Nil
    } ::
    new FlotSerie () {
      override val label = Some ("Serie 3")
      override val data = Nil
    } ::
    Nil

  // manejo listeners
  val listeners = new HashSet[Actor]

  def notifyListeners (newData : FlotNewData) = {
    listeners.foreach(_ ! newData)
  }


  def act() = {
    loop {
      react {

        // nueva muestra
        case muestra : Muestra => {

          // actualiza series flot
          val seq = for (z <- series zip muestra.medidas) yield {
            new FlotSerie () {
              override val label = z._1.label
              override val data = z._1.data.takeRight (max) ::: List ((0.0 + muestra.time, z._2))
              }
            }

          series = seq.toList

          val newDatas = (for (medida <- muestra.medidas) yield (0.0 + muestra.time, medida)).toList

          // avisa de la nueva muestra para los listeners ya incorpotados
          notifyListeners (FlotNewData (series, newDatas))
        }

        // agrega un listener
        case AddListener(listener: Actor) => {
          listeners.incl(listener)
          // retorna las series de flot
          reply (FlotInfo ("", series, options))
        }

        // saca un listener
        case RemoveListener(listener: Actor) =>
          listeners.excl(listener)
      }
    }
  }
}

// simula un sensor enviando cada 2 segundos una muestra con 3 mediciones cada una

object Sensor extends java.lang.Runnable {
  val acum = new AcumMuestrasActor (10)

  def start () = {
    acum.start

    new Thread (this).start
  }

  override def run () : Unit = {
    while (true)
    {
      val time = new java.util.Date ().getTime ()

      val sinus = Math.sin (time)
      val cosinus = Math.cos (time)
      val both = sinus + 2.0 * cosinus

      acum ! Muestra (time, List (sinus, cosinus, both))

      Thread.sleep (2000)
    }
  }
}
