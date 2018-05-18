package user_management

import java.util

import accounting.{Accounting, BookBrief}
import com.mongodb.client.model.UpdateOptions
import contracts.{BotContract, BotContractSpec, Contracts}
import helpers.Helpers._
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonString, conversions}
import org.mongodb.scala.model.Filters._
import token_management.{LoginSpec, TokenManagement}

import scala.collection.mutable.ListBuffer

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
                var email: String,
                var phone: String,
                region: String,
                role: String,
                var passwordHash: String)

case class UserSpec(name: String,
                    surname: String,
                    username: String,
                    email: String,
                    phone: String,
                    region: String,
                    role: String,
                    passwordHash: String,
                    botContracts: util.ArrayList[BotContractSpec])

case class UserUpdateSpec(email: String, phone: String, note: String)
case class PasswordResetSpec(oldPassword: String, newPassword: String, note: String)

object UserExposed {
  def apply(user: User): UserExposed = {
    val botContracts = Contracts.getContractsOf(user.username)
    val bookBriefs = Accounting.getBookBriefs(user.username)

    new UserExposed(user.name, user.surname, user.username, user.email, user.phone, user.region, user.role,
      botContracts, bookBriefs)
  }

  def apply(username: String): UserExposed = {
    val user = UserManagement.getByUsername(username)
    UserExposed(user)
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

case class UserSearchCriteria(userIds: Option[List[String]] = None, regions: Option[List[String]] = None)



object UserManagement {
  val usersCollection = CleaMongoClient.getUsersCollection

  val admin = new User(new ObjectId().toString, "Admin", "Admin", "admin", "admin@talisant.com", "+37493223775",
    "arm", "admin", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC")
  val talisant = new User(new ObjectId().toString, "Talisant", "Talisnt", "talisant", "talisant@talisant.com", "+37493223775",
    "arm", "client", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC")

  // Methods

  def setup {
    val admins = usersCollection.find(equal("username", "admin")).first().results()

    if (admins.isEmpty) {
      val user = usersCollection.insertOne(admin).results()
    }

    val talisants = usersCollection.find(equal("username", "talisant")).first().results()

    if (talisants.isEmpty) {
      usersCollection.insertOne(talisant).results()
      Accounting.createBook(talisant.username, "profit")
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
    if (!TokenManagement.isTokenBlacklisted(token))
      TokenManagement.blacklistToken(token)
  }

  // create user
  def createUser(userSpec: UserSpec) = {
    val newUser = User(userSpec)

    usersCollection.insertOne(newUser).results()
    val insertedUser = usersCollection.find(equal("username", userSpec.username)).first().results()(0)
    userSpec.botContracts.forEach(Contracts.createContract(insertedUser.username, _))

    Accounting.createBook(insertedUser.username, "profit")
    userSpec.botContracts forEach (contract => Accounting.createBook(userSpec.username, contract.botName))

    insertedUser
  }

  def getByUsername(username: String) = {
    usersCollection.find(equal("username", username)).first().results()(0)
  }

  def deleteUser(username: String) = {
    usersCollection.deleteOne(equal("username", username)).results()
    Accounting.deleteBooks(username)
    Contracts.deleteContracts(username)
  }

  def updateUser(username: String, userUpdateSpec: UserUpdateSpec) = {
    val user = UserManagement.getByUsername(username)

    if(userUpdateSpec.email != null)
      user.email = userUpdateSpec.email

    if(userUpdateSpec.phone != null)
      user.phone = userUpdateSpec.phone

    UserManagement.save(user)
  }

  def getUsers(userSearchCriteria: UserSearchCriteria) = {
    val filters = new ListBuffer[conversions.Bson]()

    userSearchCriteria.regions match {
      case Some(regions) => filters += in("region", regions:_*)
      case None => ;
    }

    userSearchCriteria.userIds match {
      case Some(userIds) => filters += in("username", userIds:_*)
      case None => ;
    }

    if (!filters.isEmpty)
      usersCollection.find(and(filters: _*)).results()
    else usersCollection.find().results()
  }

  def save(user: User) =
    usersCollection.replaceOne(equal("username", user.username), user, new UpdateOptions().upsert(true)).results()

  def changePassword(username: String, passwordResetSpec: PasswordResetSpec) = {

  }
}
