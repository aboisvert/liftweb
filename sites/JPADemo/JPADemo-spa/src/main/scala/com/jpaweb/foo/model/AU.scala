package com.foo.jpaweb.model

import java.util.Currency;
import java.util.Locale;
import java.text.NumberFormat;

/* Australian Money */
object AU extends CurrencyZone {
  type Currency = AUD

  abstract class AUD extends AbstractCurrency {
    override val designation = "AUD"
    override val numberOfFractionDigits = 2
    override val currencySymbol = "$"
    override val auLocale = new Locale("en", "AU")
    override val scale = 10
  }

  def make(x: BigDecimal) = new AUD {
    val amount = x
  }

  def apply(x: BigDecimal): AUD = make(x)
  def apply(x: String): AUD = make(BigDecimal(x))
  def apply(): AUD = make(BigDecimal(0))

  val Cent = make(.01)
  val Dollar = make(1)
  val CurrencyUnit = Dollar
}
