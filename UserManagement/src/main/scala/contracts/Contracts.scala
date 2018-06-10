package contracts

import java.util

import accounting.Accounting
import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import mongo.CleaMongoClient
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import helpers.Validators
import org.bson.types.ObjectId

/**
  * Created by spectrum on 5/15/2018.
  */
case class BotContractSpec(botName: String, profitMargin: Float) {
  def isValid = botName != null && Validators.isValidBotName(botName) && 0 <= profitMargin && profitMargin <= 100
}

case class BotContract(_id: String, userId: String, botName: String, var profitMargin: Float, createdAt: Long)

object BotContract {
  def apply(username: String, botContractSpec: BotContractSpec): BotContract =
    BotContract(new ObjectId().toString, username, botContractSpec.botName, botContractSpec.profitMargin / 100, System.currentTimeMillis())
}

class Contracts

object Contracts {
  val logger = Logger[Contracts]
  val contractsCollection = CleaMongoClient.getContractsCollection

  def getContractsOf(userId: String) = {
    val contracts = new util.ArrayList[BotContract]()
    val rawContracts = contractsCollection.find(and(equal("userId", userId))).results()

    if (rawContracts != null && !rawContracts.isEmpty)
      rawContracts foreach (contracts.add)

    contracts
  }

  def getContractByBot(botName: String) = contractsCollection.find(equal("botName", botName)).results()

  def getContract(username: String, botName: String) = {
    val contracts = contractsCollection.find(and(equal("userId", username), equal("botName", botName))).first().results()

    if (contracts.nonEmpty)
      contracts(0)
    else
      null
  }

  def createContract(username: String, contractSpec: BotContractSpec) = {
    val contracts =
      contractsCollection.find(and(equal("username", username), equal("botName", contractSpec.botName))).first().results()

    if (contracts != null && contracts.isEmpty)
      contractsCollection.insertOne(BotContract(username, contractSpec)).results()

    val talisantContracts =
      contractsCollection.find(and(equal("username", "talisant"), equal("botName", contractSpec.botName))).results()

    if (talisantContracts != null && !talisantContracts.isEmpty) {
      val booksOfInterest = Accounting.getBooksByName(contractSpec.botName) filter {
        _.balance > 0
      }
      if (booksOfInterest.nonEmpty) {
        val contract = talisantContracts(0)
        contract.profitMargin = (contract.profitMargin + (1 - contractSpec.profitMargin / 100)) / booksOfInterest.size
        contractsCollection.replaceOne(and(equal("username", "talisant"), equal("botName", contractSpec.botName)),
          contract, new UpdateOptions().upsert(true))
      } else {
        logger.error("No parties partake in this bot profit. Something wrong happened")
      }
    } else {
      contractsCollection.insertOne(BotContract("talisant", BotContractSpec(contractSpec.botName, 100 - contractSpec.profitMargin))).results()
    }
  }

  def deleteContract(username: String, botName: String) = {
    val userContracts = contractsCollection.find(and(equal("userId", username), equal("botName", botName))).results()

    if (userContracts != null && !userContracts.isEmpty) {
      val userContract = userContracts(0)
      contractsCollection.deleteOne(and(equal("userId", username), equal("botName", botName))).results()
      val contracts = contractsCollection.find(and(equal("userId", "talisant"), equal("botName", botName))).results()

      if (contracts != null && !contracts.isEmpty) {
        val contract = contracts(0)
        contract.profitMargin = contract.profitMargin - (1 - userContract.profitMargin)
        contractsCollection.replaceOne(and(equal("userId", "talisant"), equal("botName", botName)), contract, new UpdateOptions().upsert(true))
      }
    }
  }

  def deleteContracts(username: String) = {
    val userContracts = contractsCollection.find(equal("userId", username)).results()

    if (userContracts != null) {
      userContracts foreach { c => deleteContract(username, c.botName) }
    }
  }
}