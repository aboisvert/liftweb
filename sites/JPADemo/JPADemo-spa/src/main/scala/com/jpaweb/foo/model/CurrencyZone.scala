
package com.foo.jpaweb.model

import java.util.Currency;
import java.util.Locale;
import java.text.NumberFormat;

/* currency factory*/
abstract class CurrencyZone {
  type Currency <: AbstractCurrency

  def make(x: BigDecimal): Currency

  abstract class AbstractCurrency {
    val amount: BigDecimal
    val designation: String
    val currencySymbol: String
    val numberOfFractionDigits: Int
    val auLocale: Locale
    val scale: Int

    def +(that: Currency): Currency = this + that

    def *(that: Currency): Currency = this * that

    def -(that: Currency): Currency = this - that

    def /(that: Currency): Currency =
    make(new BigDecimal(this.amount.bigDecimal.divide(that.amount.bigDecimal, scale, java.math.BigDecimal.ROUND_HALF_UP)) )

    override def toString = format("", numberOfFractionDigits)

    def format: String = format(currencySymbol, numberOfFractionDigits)

    def format(currencySymbol: String, numberOfFractionDigits: Int): String = {
      var moneyValue = amount
      if (amount == null) moneyValue = 0

      moneyValue = moneyValue.setScale(numberOfFractionDigits, BigDecimal.RoundingMode.ROUND_HALF_UP);
      val numberFormat = NumberFormat.getInstance(auLocale);
      numberFormat.setMinimumFractionDigits(numberOfFractionDigits);
      numberFormat.setMaximumFractionDigits(numberOfFractionDigits);
      if (moneyValue.doubleValue() < 0) return "-"+currencySymbol+numberFormat.format(moneyValue.abs.doubleValue());
      else return currencySymbol+numberFormat.format(moneyValue.doubleValue());
    }

    def get: String = get(numberOfFractionDigits)

    def get(numberOfFractionDigits: Int): String = format("", numberOfFractionDigits).replaceAll(",", "");

  }
  val CurrencyUnit: Currency
}

