package net.liftweb.paypal

import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.Assert._

import net.liftweb.paypal._


class TestPaypal {
  
  @Before
  def init = {
    println("### Starting Paypal Test...")
  }
  
  @Test
  def attemptToSetMode = {
    var p: PaypalDataTransfer = PaypalDataTransfer("dfgdf","dfgdf")
    println("######## MODE:")
    println(p.mode)
  }
  
}