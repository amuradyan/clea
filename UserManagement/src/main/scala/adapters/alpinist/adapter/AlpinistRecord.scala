package adapters.alpinist.adapter

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

/**
  * Created by spectrum on 5/22/2018.
  */

case class AlpinistRecord(amount: Float, orderOpenId: Long, orderOpenPrice: Float, orderOpenDate: LocalDateTime,
                          orderCloseId: Long, orderClosePrice: Float, orderCloseDate: LocalDateTime)

object AlpinistRecord {
  val formatter = DateTimeFormatter.ofPattern("EEE MMM dd yyyy HH:mm:ss")
  val GMTSuffix = " GMT+0400 (Caucasus Standard Time)"

  def apply(input: Array[String]) = {
    val indexOfCloseGMT = input(6) indexOf GMTSuffix
    val closeDateString = input(6) substring  (0, indexOfCloseGMT)
    val closeDate = LocalDateTime parse(closeDateString, formatter)

    val indexOfOpenGMT = input(3) indexOf GMTSuffix
    val openDateString = input(3) substring (0, indexOfOpenGMT)
    val openDate = LocalDateTime parse(openDateString, formatter)

    new AlpinistRecord(input(0).toFloat, input(1).toLong, input(2).toFloat, openDate, input(4).toLong, input(5).toFloat, closeDate)
  }
}

