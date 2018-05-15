package contracts

import com.typesafe.scalalogging.Logger

/**
  * Created by spectrum on 5/15/2018.
  */
case class BotContract(userId: String, botName: String, profitMargin: Float)

class Contracts
object Contracts {
  val logger = Logger[Contracts]

  def getContractsOf(userId: String) = ???
  def createContract(userId: String, contract: BotContract) = ???
  def deleteContract(userId: String, contract: BotContract) = ???
}