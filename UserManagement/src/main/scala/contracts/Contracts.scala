package contracts

import java.util

import com.typesafe.scalalogging.Logger
import mongo.CleaMongoClient
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import org.bson.types.ObjectId

/**
  * Created by spectrum on 5/15/2018.
  */
case class BotContractSpec(botName: String, profitMargin: Float)

case class BotContract(_id: String, userId: String, botName: String, profitMargin: Float)
object BotContract {
  def apply(userId: String, botContractSpec: BotContractSpec): BotContract =
    BotContract(new ObjectId().toString, userId, botContractSpec.botName, botContractSpec.profitMargin)
}

class Contracts
object Contracts {
  val logger = Logger[Contracts]
  val contractsCollection = CleaMongoClient.getContractsCollection

  def getContractsOf(userId: String) = {
    val contracts = new util.ArrayList[BotContract]()
    val rawContracts = contractsCollection.find(and(equal("userId", userId))).results()

    if (rawContracts != null && !rawContracts.isEmpty)
      rawContracts foreach(contracts.add)

    contracts
  }

  def createContract(userId: String, contractSpec: BotContractSpec) = {
      val contracts =
        contractsCollection.find(and(equal("userId", userId), equal("botName", contractSpec.botName))).first().results()

      if( contracts != null && contracts.isEmpty )
        contractsCollection.insertOne(BotContract(userId, contractSpec)).results()
  }

  def deleteContract(userId: String, contractSpec: BotContractSpec) =
    contractsCollection.deleteOne(and(equal("userId", userId), equal("botName", contractSpec.botName))).results()

  def deleteContracts(username: String) = contractsCollection.deleteMany(equal("userId", username)).results()
}