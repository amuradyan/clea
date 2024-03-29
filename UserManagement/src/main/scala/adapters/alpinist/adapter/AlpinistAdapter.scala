package adapters.alpinist.adapter

import java.util

import accounting.Accounting
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import helpers.Helpers._
import mongo.CleaMongoClient
import org.mongodb.scala.model.Filters._
import scalaj.http.{Http, HttpOptions}

/**
  * Created by spectrum on 5/22/2018.
  */
case class AlpinistTicker(symbol: String,
                          bid: Float,
                          ask: Float,
                          mts: Long)

case class AlpinistOrderV1(id: Long,
                         price: Float,
                         amount: Float) {
}

case class AlpinistDeal(_id: String,
                        trader: String,
                        symbol: String,
                        tickerOpen: AlpinistTicker,
                        amount: Float,
                        dateOpened: String,
                        orderOpen: AlpinistOrderV1,
                        dateClosed: String,
                        orderClose: AlpinistOrderV1,
                        tickerClose: AlpinistTicker) {
  def getProfit = (orderClose.price - orderOpen.price) * (-1 * orderClose.amount)
}

class AlpinistAdapter

object AlpinistAdapter {
  val logger = Logger[AlpinistAdapter]
  val alpinistRecordPointerCollection = CleaMongoClient.getAlpinistRecordPointerCollection

  def run = {
    var lastDate = ""
    val pointer = alpinistRecordPointerCollection.find().first().results()

    if (!pointer.isEmpty)
      lastDate = pointer(0).pos

    val deals = AlpinistAdapter.getDeals filter { d => d.dateClosed != null && d.dateClosed != "" }

    val dealsOfInterest = {
      if (lastDate != "") {
        deals filter {
          _.dateClosed > lastDate
        }
      } else
        deals
    } sortWith (_.dateClosed > _.dateClosed)

    if (dealsOfInterest.nonEmpty) {
      if (!pointer.isEmpty) {
        pointer(0).pos = dealsOfInterest(0).dateClosed
        alpinistRecordPointerCollection.replaceOne(equal("_id", pointer(0)._id), pointer(0), new UpdateOptions().upsert(true)).results()
      }
      else
        alpinistRecordPointerCollection.insertOne(AlpinistRecordPointer(dealsOfInterest.last.dateClosed)).results()


      val dealsGrouped = dealsOfInterest groupBy {
        _.orderClose.id
      }
      var totalProfit = 0f

      dealsGrouped foreach {
        group => {
          var lossTotal = 0f
          group._2 foreach {
            deal => {
              lossTotal = lossTotal + deal.orderOpen.amount * deal.orderOpen.price
              logger.info(s"  Loss for order : ${deal.orderOpen.amount * deal.orderOpen.price}")
              logger.info(s"  Total loss : $lossTotal")
            }
          }

          val profit = (group._2(0).orderClose.amount * group._2(0).orderClose.price * -1) - lossTotal
          totalProfit = totalProfit + profit

          logger.info(s"Profit for group : $profit")
          logger.info(s"Total profit : $totalProfit")
        }
      }

      Accounting.distributeProfit(totalProfit, "alpinist")
    }
  }

  private def getDeals = {
    var dealList = List[AlpinistDeal]()
    val url = "http://alpinist.prism.melbourne/api/v1/records/?trader=5af3f805224ded00291a8bef"

    try {
      val dealsResponse = Http(url)
        .header("Content-Type", "application/json")
        .header("Charset", "UTF-8")
        .option(HttpOptions.readTimeout(10000))
        .option(HttpOptions.allowUnsafeSSL).asString

      if (dealsResponse.code == 200) {
        val dealsJson = dealsResponse.body.toString

        val token = new TypeToken[util.ArrayList[AlpinistDeal]]() {}.getType
        val deals: util.ArrayList[AlpinistDeal] = new Gson().fromJson(dealsJson, token)

        deals forEach { e => dealList :+= e }
      }
    } catch {
      case _: Throwable => logger.error(s"Unable to fetch orders from Alpinist at : $url")
    }

    dealList
  }

  def main(args: Array[String]) {
    run
  }
}
