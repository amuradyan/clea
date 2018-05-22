package importers

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import accounting.Accounting
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import util.control.Breaks._

case class AlpinistRecord(amount: Float, orderOpenId: Long, orderOpenPrice: Float, orderOpenDate: LocalDate,
                          orderCloseId: Long, orderClosePrice: Float, orderCloseDate: LocalDate)

object AlpinistRecord {
  val formatter = DateTimeFormatter.ofPattern("EEE MMM dd yyyy HH:mm:ss")
  val GMTSuffix = " GMT+0400 (Caucasus Standard Time)"

  def apply(input: Array[String]) = {
    val indexOfCloseGMT = input(6) indexOf GMTSuffix
    val closeDateString = input(6) substring  (0, indexOfCloseGMT)
    val closeDate = LocalDateTime parse(closeDateString, formatter) toLocalDate

    val indexOfOpenGMT = input(3) indexOf GMTSuffix
    val openDateString = input(3) substring (0, indexOfOpenGMT)
    val openDate = LocalDateTime parse(openDateString, formatter) toLocalDate

    new AlpinistRecord(input(0).toFloat, input(1).toLong, input(2).toFloat, openDate, input(4).toLong, input(5).toFloat, closeDate)
  }
}

class AlpinistHistoryImporter

object AlpinistHistoryImporter {
  val logger = Logger[AlpinistHistoryImporter]

  private def getDailyProfit(records: Iterable[AlpinistRecord]) = {
    var dailyProfit = 0f

    records foreach {
      r => dailyProfit = dailyProfit + (r.orderClosePrice - r.orderOpenPrice) * r.amount
    }

    dailyProfit
  }

  def main(args: Array[String]) {
    val sources = List(
      "E:\\playground\\cryptoarm\\Clea\\alipinst_history\\records.csv",
      "E:\\playground\\cryptoarm\\Clea\\alipinst_history\\records(1).csv",
      "E:\\playground\\cryptoarm\\Clea\\alipinst_history\\records(2).csv",
      "E:\\playground\\cryptoarm\\Clea\\alipinst_history\\records(3).csv",
      "E:\\playground\\cryptoarm\\Clea\\alipinst_history\\records(6).csv")

    val allRecordsRaw = mutable.Map[Long, AlpinistRecord]()

    sources foreach {
      src => {
        val bufferedSource = io.Source.fromFile(src)
        var lineNumber = 0

        for (line <- bufferedSource getLines)
        {
          breakable {
            if(lineNumber == 0) {
              lineNumber = lineNumber + 1
              break
            }

            val cols = line split (",") map (_.trim)

            if (cols.length == 7) {
              val record = AlpinistRecord(cols)
              allRecordsRaw(cols(1) toLong) = record
            } else
              logger.error(s"Invalid record : $cols at line $lineNumber in $src")

            lineNumber = lineNumber + 1
          }
        }
      }
    }

    val recordsGroupedByCloseId = allRecordsRaw map (_._2) groupBy (_.orderCloseDate)

    recordsGroupedByCloseId foreach {
      group => {
        val dailyProfit = getDailyProfit(group._2)
        Accounting.distributeProfit(dailyProfit, "alpinist")
      }
    }
  }
}
