package adapters.cleversniper

import java.util

import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import com.typesafe.scalalogging.Logger

import scalaj.http.{Http, HttpOptions}

/**
  * Created by spectrum on 5/13/2018.
  */
case class Order(order_number: String,
                 pair: String,
                 lot: Float,
                 pl: Float,
                 open_price: Float,
                 current_price: Float,
                 `type`: String,
                 is_open: Boolean,
                 exchange: String,
                 amount: Float,
                 info: String,
                 last_updated: String,
                 commission: Float)

case class CleversniperDeal(ID: String, Profit: Float, CreatedAt: String, LastUpdated: String,
                            BuyOrder: Order, SellOrder: Order)

class CleversniperAdapter
object CleversniperAdapter {
  val logger = Logger[CleversniperAdapter]

  def getDeals: List[CleversniperDeal] = {
    var dealList = List[CleversniperDeal]()

    try {
      val tokenResponse = Http("https://algo.cleversniper.com/api/session").option(HttpOptions.allowUnsafeSSL).asString

      if (tokenResponse.code == 200) {
        val dealsResponse = Http("https://algo.cleversniper.com/api/orders?removeall=false")
          .header("Content-Type", "application/json")
          .header("Charset", "UTF-8")
          .header("sessionid", tokenResponse.body.toString.replace("\"", ""))
          .option(HttpOptions.readTimeout(10000))
          .option(HttpOptions.allowUnsafeSSL).asString

        if (dealsResponse.code == 200) {
          val dealsJson = dealsResponse.body.toString

          val token = new TypeToken[util.ArrayList[CleversniperDeal]]() {}.getType

          val deals: util.ArrayList[CleversniperDeal] = new Gson().fromJson(dealsJson, token)

          deals.forEach({ el => dealList :+= el })
        } else {
          logger.info("Unable to obtain deals from adapters.cleversniper")
        }
      } else {
        logger.info("Unable to obtain a token from adapters.cleversniper")
      }
    } catch {
      case _ => logger.error("Unable to fetch data from ")
    }

    dealList
  }
}
