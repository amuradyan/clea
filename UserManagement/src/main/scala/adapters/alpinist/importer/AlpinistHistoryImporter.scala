package adapters.alpinist.importer

import java.time.{LocalDateTime, ZoneOffset}

import accounting.Accounting
import adapters.alpinist.adapter.AlpinistRecord
import com.typesafe.scalalogging.Logger

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.control.Breaks._

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

    implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))

    val recordsGroupedByCloseId = allRecordsRaw map (_._2) groupBy (_.orderCloseDate)
    val sortedRecordGroups = ListMap(recordsGroupedByCloseId.toSeq.sortBy(_._1):_*)

    sortedRecordGroups foreach {
      group => {
        val dailyProfit = getDailyProfit(group._2)
        Accounting.distributeProfit(dailyProfit, "alpinist", group._2.head.orderCloseDate)
      }
    }
  }
}
