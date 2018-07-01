package adapters.alpinist.adapter

import java.time.LocalDateTime
import java.util

import accounting.Accounting
import com.google.gson.Gson
import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import helpers.Helpers._
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters.equal
import scalaj.http.{Http, HttpOptions}

/**
  * Created by spectrum on Jun, 2018
  */
case class AlpinistOrder(buy_price: Float,
                         sell_price: Float,
                         quantity: Float,
                         broker: String,
                         sell_time: Long)

case class AlpinistResponse(docs: util.ArrayList[AlpinistOrder] = new util.ArrayList[AlpinistOrder](),
                            bookmark: String = "",
                            warning: String = "")

case class Selector(side: String = "sell",
                    sell_status: String = "closed")

case class AlpinistQuery(selector: Selector = Selector(),
                         fields: util.ArrayList[String] = new util.ArrayList()) {
  def apply: AlpinistQuery = {
    val fields = new util.ArrayList[String]()
    fields.add("buy_price")
    fields.add("sell_price")
    fields.add("quantity")
    fields.add("broker")
    fields.add("sell_time")
    AlpinistQuery(Selector(), fields)
  }
}

case class AlpinistRecordPointer(_id: String, var pos: String)

object AlpinistRecordPointer {
  def apply(pos: String): AlpinistRecordPointer = new AlpinistRecordPointer(new ObjectId().toString, pos)
}

class AlpinistAdapterV2

object AlpinistAdapterV2 {
  private val logger = Logger[AlpinistAdapterV2]
  private val alpinistRecordPointerCollection = CleaMongoClient.getAlpinistRecordPointerCollection

  def run: Unit = {
    var lastDate = ""
    val pointer = alpinistRecordPointerCollection.find().first().results()

    if (!pointer.isEmpty)
      lastDate = pointer(0).pos

    val deals = AlpinistAdapterV2.getDeals

    val dealsOfInterest = {
      if (lastDate != "") {
        deals filter {
          _.sell_time > lastDate.toLong
        }
      } else
        deals
    } sortWith (_.sell_time > _.sell_time)

    if (dealsOfInterest.nonEmpty) {
      if (!pointer.isEmpty) {
        pointer(0).pos = dealsOfInterest(0).sell_time.toString
        alpinistRecordPointerCollection.replaceOne(equal("_id", pointer(0)._id), pointer(0), new UpdateOptions().upsert(true)).results()
      }
      else
        alpinistRecordPointerCollection.insertOne(AlpinistRecordPointer(dealsOfInterest.last.sell_time.toString)).results()

      var totalProfit = 0f

      deals foreach {
        d => {
          if (d.broker == "cexio")
            totalProfit = totalProfit + d.quantity * 0.25f * (d.sell_price - d.buy_price)
          else if (d.broker == "bitfinex")
            totalProfit = totalProfit + d.quantity * 0.2f * (d.sell_price - d.buy_price)
          else
            logger.error(s"Unsupported exchange ${d.broker}")
        }
      }

      Accounting.distributeProfit(totalProfit, "alpinist")
    }
  }

  private def getDeals = {
    val url = "http://ec2-13-59-155-218.us-east-2.compute.amazonaws.com:5984/orders/_find"
    var dealList = List[AlpinistOrder]()

    val fields = new util.ArrayList[String]()
    fields.add("buy_price")
    fields.add("sell_price")
    fields.add("quantity")
    fields.add("broker")
    fields.add("sell_time")

    try {
      val dealsResponse = Http(url)
        .header("Content-Type", "application/json")
        .header("Charset", "UTF-8")
        .postData(new Gson().toJson(AlpinistQuery(Selector(), fields)))
        .option(HttpOptions.readTimeout(10000))
        .option(HttpOptions.allowUnsafeSSL).asString

      if (dealsResponse.code == 200) {
        val dealsJson = dealsResponse.body.toString
        val deals = new Gson().fromJson(dealsJson, classOf[AlpinistResponse])

        deals.docs forEach { e => dealList :+= e }
      }
    } catch {
      case e: Exception => e.printStackTrace()
      case _: Throwable => logger.error(s"Unable to fetch orders from Alpinist at : $url")
    }

    logger.info(s"Fetched ${dealList.length} deals via AlpinistV2 at ${LocalDateTime.now()}")
    dealList
  }

  def main(args: Array[String]): Unit = {
    getDeals
  }
}
