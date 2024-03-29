package user_management

import java.util

import accounting.{Accounting, BookBrief}
import com.mongodb.client.model.UpdateOptions
import contracts.{BotContract, BotContractSpec, Contracts}

import scala.util.control.Breaks._
import helpers.Helpers._
import helpers.Validators
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.bson.conversions
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import token_management.{LoginSpec, TokenManagement}

import scala.collection.mutable.ListBuffer

final class UserNotFound extends Throwable

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
                    var passwordHash: String,
                    botContracts: util.ArrayList[BotContractSpec]) {
  def isValid = {
    name != null && name.nonEmpty &&
      surname != null && surname.nonEmpty &&
      username != null && username.nonEmpty &&
      email != null && Validators.isEmail(email) &&
      phone != null && Validators.isPhoneNumber(phone) &&
      region != null && Validators.isRegion(region)
    role != null && Validators.isRole(role) &&
      passwordHash != null && passwordHash.nonEmpty &&
      areContractsValid(botContracts)
  }

  private def areContractsValid(botContracts: util.ArrayList[BotContractSpec]) = {
    var res = true

    breakable {
      botContracts forEach {
        contract => {
          if (!contract.isValid) {
            res = false
            break
          }
        }
      }
    }

    res
  }
}

case class UserUpdateSpec(email: String, phone: String, note: String = "") {
  def isValid = (email != null && Validators.isEmail(email)) || (phone != null && Validators.isPhoneNumber(phone))
}

case class PasswordResetSpec(oldPassword: String, newPassword: String, note: String = "") {
  def isValid = oldPassword != null && oldPassword.nonEmpty && newPassword != null && newPassword.nonEmpty
}

object UserExposed {
  def apply(user: User): UserExposed = {
    val botContracts = Contracts.getContractsOf(user.username)
    val bookBriefs = Accounting.getBookBriefs(user.username)

    new UserExposed(user.name, user.surname, user.username, user.email, user.phone, user.region, user.role,
      botContracts, bookBriefs)
  }

  def apply(username: String): UserExposed = {
    UserManagement.getByUsername(username) match {
      case Some(u) => UserExposed(u)
      case None => throw new UserNotFound
    }
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

    if (admins.isEmpty)
      usersCollection.insertOne(admin).results()

    val talisants = usersCollection.find(equal("username", "talisant")).first().results()

    if (talisants.isEmpty) {
      usersCollection.insertOne(talisant).results()
      Accounting.createBook(talisant.username, "profit")
    }
  }

  def login(loginSpec: LoginSpec) = {
    val users = usersCollection.find(and(equal("username", loginSpec.username), equal("passwordHash", loginSpec.passwordHash.toUpperCase))).first().results()

    if (!users.isEmpty)
      Some(TokenManagement.issueToken(users(0)))
    else
      None
  }

  def logout(token: String) = {
    if (!TokenManagement.isTokenBlacklisted(token))
      TokenManagement.blacklistToken(token)
  }

  def createUser(userSpec: UserSpec) = {
    userSpec.passwordHash = userSpec.passwordHash.toUpperCase
    val newUser = User(userSpec)

    usersCollection.insertOne(newUser).results()
    val insertedUser = getByUsername(userSpec.username)

    insertedUser match {
      case Some(u) => {
        Accounting.createBook(u.username, "profit")

        if (userSpec.botContracts != null) {
          userSpec.botContracts forEach {
            contract => Contracts.createContract(u.username, contract)
          }

          val talisantProfitBook = Accounting.getBook("talisant", "profit")

          talisantProfitBook match {
            case None => Accounting.createBook("talisnat", "profit")
            case _ => ;
          }

          userSpec.botContracts forEach (contract => Accounting.createBook(userSpec.username, contract.botName))
        }

        insertedUser
      }
      case None => throw new UserNotFound;
    }
  }

  def getByUsername(username: String): Option[User] = {
    val users = usersCollection.find(equal("username", username)).first().results()

    if(users.nonEmpty) Some(users(0))
    else None
  }

  def deleteUser(username: String) = {
    usersCollection.deleteOne(equal("username", username)).results()
    Accounting.deleteBooks(username)
    Contracts.deleteContracts(username)
  }

  def updateUser(username: String, userUpdateSpec: UserUpdateSpec) = {
    val user = UserManagement.getByUsername(username)

    user match {
      case Some(u) => {
        if (userUpdateSpec.email != null)
          u.email = userUpdateSpec.email

        if (userUpdateSpec.phone != null)
          u.phone = userUpdateSpec.phone

        val updateResult = UserManagement.save(u)
        if (updateResult.nonEmpty) updateResult.head else null
      }
      case None => throw new UserNotFound
    }
  }

  def getUsers(userSearchCriteria: UserSearchCriteria) = {
    val filters = new ListBuffer[conversions.Bson]()

    userSearchCriteria.regions match {
      case Some(regions) => filters += in("region", regions map {_.replaceAll("\\+", "")} : _*)
      case None => ;
    }

    userSearchCriteria.userIds match {
      case Some(userIds) => filters += in("username", userIds map {_.replaceAll("\\+", "")} : _*)
      case None => ;
    }

    if (!filters.isEmpty)
      usersCollection.find(and(filters: _*)).results()
    else usersCollection.find().results()
  }

  def getAllUsers = usersCollection.find().results

  def save(user: User) =
    usersCollection.replaceOne(equal("username", user.username), user, new UpdateOptions().upsert(true)).results()

  def get_SHA_512_SecurePassword(passwordToHash: String): String = {
    import java.nio.charset.StandardCharsets
    import java.security.MessageDigest
    import java.security.NoSuchAlgorithmException

    var generatedPassword: String = null
    try {
      val md = MessageDigest.getInstance("SHA-512")
      val bytes = md.digest(passwordToHash.getBytes(StandardCharsets.UTF_8))
      val sb = new StringBuilder
      var i = 0
      while ( {
        i < bytes.length
      }) {
        sb.append(Integer.toString((bytes(i) & 0xff) + 0x100, 16).substring(1))

        {
          i += 1; i - 1
        }
      }
      generatedPassword = sb.toString
    } catch {
      case e: NoSuchAlgorithmException =>
        e.printStackTrace()
    }
    generatedPassword
  }

  def changePassword(username: String, passwordResetSpec: PasswordResetSpec){
    UserManagement.getByUsername(username) match {
      case Some(u) => {
        val oldPasswordHash = get_SHA_512_SecurePassword(passwordResetSpec.oldPassword).toUpperCase
        if (oldPasswordHash.equals(u.passwordHash)){
          val newPasswordHash = get_SHA_512_SecurePassword(passwordResetSpec.newPassword).toUpperCase
          usersCollection.findOneAndUpdate(equal("username", username), set("passwordHash", newPasswordHash)).results()
        }
      }
      case None => throw new UserNotFound
    }
  }
}
