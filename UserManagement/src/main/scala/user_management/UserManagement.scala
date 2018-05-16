package user_management

import java.util

import accounting.{Accounting, BookBrief}
import contracts.{BotContract, BotContractSpec, Contracts}
import helpers.Helpers._
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString}
import org.mongodb.scala.model.Filters._
import token_management.{LoginSpec, TokenManagement}

import collection.JavaConverters._

/**
  * Created by spectrum on 5/14/2018.
  */
object User {
  def apply(userSpec: UserSpec): User = new User(new ObjectId().toString, userSpec.name, userSpec.surname,
      userSpec.username, userSpec.email, userSpec.phone, userSpec.region, userSpec.role, userSpec.passwordHash)
}

case class User(_id: String,
                name: String,
                surname: String,
                username: String,
                email: String,
                phone: String,
                region: String,
                role: String,
                passwordHash: String)

case class UserSpec(name: String,
                    surname: String,
                    username: String,
                    email: String,
                    phone: String,
                    region: String,
                    role: String,
                    passwordHash: String,
                    botContracts: util.ArrayList[BotContractSpec])

object UserExposed {
  def apply(user: User): UserExposed = {
    val botContracts = Contracts.getContractsOf(user._id)
    val bookBriefs = Accounting.getBookBriefs(user._id)

    new UserExposed(user.name, user.surname, user.username, user.email, user.phone, user.region, user.role,
      botContracts, bookBriefs)
  }
}

case class UserExposed(name: String,
                       surname: String,
                       username: String,
                       email: String,
                       phone: String,
                       region: String,
                       role: String,
                       botContracts: util.ArrayList[BotContract],
                       bookBriefs: util.ArrayList[BookBrief])

case class UserSearchCriteria(userIds: Option[List[String]] = None, region: Option[String] = None)


object UserManagement {
  val usersCollection = CleaMongoClient.getUsersCollection

  val admin = new User(new ObjectId().toString, "Admin", "Admin", "admin", "admin@talisant.com", "+37493223775",
    "ARM", "admin", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC")
  val talisant = new User(new ObjectId().toString, "Talisant", "Talisnt", "talisant", "talisant@talisant.com", "+37493223775",
    "ARM", "client", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC")

  // Methods

  def setup {
    val admins = usersCollection.find(equal("username", "admin")).first().results()

    if (admins.isEmpty) {
      val user = usersCollection.insertOne(admin).results()
    }

    val talisants = usersCollection.find(equal("username", "talisant")).first().results()

    if (talisants.isEmpty) {
      usersCollection.insertOne(talisant).results()
      Accounting.createBook(talisant._id, "profit")
    }
  }

  def login(loginSpec: LoginSpec) = {
    val users = usersCollection.find(and(equal("username", loginSpec.username), equal("passwordHash", loginSpec.passwordHash))).first().results()

    if (!users.isEmpty)
      Some(TokenManagement.issueToken(users(0)))
    else
      None
  }

  def logout(token: String) = {
    if(!TokenManagement.isTokenBlacklisted(token))
      TokenManagement.blacklistToken(token)
  }

  // create user
  def createUser(userSpec: UserSpec) = {
    val newUser = User(userSpec)

    usersCollection.insertOne(newUser).results()
    val insertedUser = usersCollection.find(equal("username", userSpec.username)).first().results()(0)
    userSpec.botContracts forEach(Contracts.createContract(insertedUser._id, _))

    Accounting.createBook(insertedUser._id, "Profit")
    userSpec.botContracts forEach(contract => Accounting.createBook(insertedUser._id, contract.botName))

    insertedUser
  }

  def getByUsername(username: String) = {
    usersCollection.find(equal("username", username)).first().results()(0)
  }

  // delete user
  def deleteUser(username: String) = {
    usersCollection.deleteOne(equal("username", username)).results()
  }

  // update user
  def updateUser(username:String, userUpdateSpec: BsonDocument) = {
    val updateQuery = new BsonDocument()
    updateQuery.append("$set", userUpdateSpec)
    usersCollection.findOneAndUpdate(equal("username", username), updateQuery).results()
    usersCollection.find(equal("username", username)).results()(0)
  }

  def getUsers(userSearchCriteria: UserSearchCriteria) = {
    val filter = BsonDocument()

    userSearchCriteria.region match {
      case Some(region) => filter.append("region", BsonString(region))
      case None => ;
    }

    userSearchCriteria.userIds match {
      case Some(userIds) => {
        val inFilter = BsonDocument()
        inFilter.append("$in", BsonArray(userSearchCriteria.userIds))
        filter.append("username", inFilter)
      }
      case None => ;
    }

    usersCollection.find(filter).results()
  }

  def changePassword = ???

//  def addContract(username: String, contract: BotContract) = {
//    var contracts = getByUsername(username).botContracts
//
//    if(!contracts.contains(contract)) {
//
//      contracts :+= contract
//      val contractsBson = BsonArray()
//
//      contracts.foreach(
//        el => {
//          val contractBson = BsonDocument()
//          contractBson.append("botName", BsonString(el.botName))
//          contractBson.append("profitMargin", BsonNumber(el.profitMargin))
//
//          contractsBson.add(contractBson)
//        }
//      )
//
//      val botContracts= BsonDocument()
//      val javaContracts = new util.ArrayList[BotContract]
//      contracts.foreach{javaContracts.add(_)}
//      botContracts.append("botContracts", contractsBson)
//      val updateQuery = new BsonDocument()
//      updateQuery.append("$set", botContracts)
//      usersCollection.findOneAndUpdate(equal("username", username), updateQuery).results()
//      usersCollection.find(equal("username", username)).results()(0)
//    }
//  }
}