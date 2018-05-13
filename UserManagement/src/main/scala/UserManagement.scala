import java.util
import java.util.concurrent.TimeUnit

import com.google.gson.Gson
import com.mongodb.ConnectionString
import com.typesafe.config.ConfigFactory
import org.bson.types.ObjectId
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonNumber, BsonString}
import org.mongodb.scala.connection.ClusterSettings
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.{Document, MongoClient, MongoClientSettings, MongoCollection, MongoCredential, Observable}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Created by spectrum on 5/4/2018.
  */
object Helpers {

  implicit class DocumentObservable[C](val observable: Observable[Document]) extends ImplicitObservable[Document] {
    override val converter: (Document) => String = (doc) => doc.toJson
  }

  implicit class UserObservable[C](val observable: Observable[User]) extends ImplicitObservable[User] {
    val gson = new Gson();
    override val converter: (User) => String = (doc) => gson.toJson(doc)
  }

  implicit class TokenObservable[C](val observable: Observable[Token]) extends ImplicitObservable[Token] {
    val gson = new Gson();
    override val converter: (Token) => String = (doc) => gson.toJson(doc)
  }

  implicit class GenericObservable[C](val observable: Observable[C]) extends ImplicitObservable[C] {
    override val converter: (C) => String = (doc) => doc.toString
  }

  trait ImplicitObservable[C] {
    val observable: Observable[C]
    val converter: (C) => String

    def results(): Seq[C] = Await.result(observable.toFuture(), Duration(10, TimeUnit.SECONDS))

    def headResult() = Await.result(observable.head(), Duration(10, TimeUnit.SECONDS))

    def printResults(initial: String = ""): Unit = {
      if (initial.length > 0) print(initial)
      results().foreach(res => println(converter(res)))
    }

    def printHeadResult(initial: String = ""): Unit = println(s"${initial}${converter(headResult())}")
  }
}

case class Token(_id: String, token: String)

object UserManagement {
  import Helpers._
  import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
  import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
  import org.mongodb.scala.bson.codecs.Macros._

  val codecRegistry = fromRegistries(fromProviders(classOf[User], classOf[BotContract], classOf[Token]), DEFAULT_CODEC_REGISTRY)

  val conf = ConfigFactory.load()

  val db = conf.getString("mongodb.db")
  val user = conf.getString("mongodb.user")
  val password = conf.getString("mongodb.password")
  val host = conf.getString("mongodb.host")
  val port = conf.getString("mongodb.port")

  val clientSettingsBuilder = MongoClientSettings.builder()
  val mongoClusterSettingsBuilder = ClusterSettings.builder()
  val mongoConnectionString = new ConnectionString(s"mongodb://$host:$port")
  mongoClusterSettingsBuilder.applyConnectionString(mongoConnectionString)

  val credential = MongoCredential.createScramSha1Credential(user, db, password.toCharArray)
  clientSettingsBuilder.credential(credential)
  clientSettingsBuilder.clusterSettings(mongoClusterSettingsBuilder.build())

  val mongoClient = MongoClient(clientSettingsBuilder.build())

  val cleaDB = mongoClient.getDatabase(db).withCodecRegistry(codecRegistry)

  val usersCollection: MongoCollection[User] = cleaDB.getCollection("users")
  val tokenCollection: MongoCollection[Token] = cleaDB.getCollection("tokens")


  var blacklistedTokens = Seq[String]()

  val admin = new User(new ObjectId().toString, "Admin", "Admin", "admin", "admin@talisant.com", "+37493223775",
    "ARM", "admin", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC", new ObjectId().toString, List())
  val talisant = new User(new ObjectId().toString, "Talisant", "Talisnt", "talisant", "talisant@talisant.com", "+37493223775",
    "ARM", "client", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC", new ObjectId().toString, List())

  // Methods

  def setup {
    val admins = usersCollection.find(equal("username", "admin")).first().results()

    if (admins.isEmpty) {
      usersCollection.insertOne(admin).results()
    }

    val talisants = usersCollection.find(equal("username", "talisant")).first().results()

    if (talisants.isEmpty) {
      usersCollection.insertOne(talisant).results()
    }
  }

  def isTokenBlacklisted(token: String) = {
    val tokens = tokenCollection.find(equal("token", token)).first().results()

    !tokens.isEmpty
  }

  // login
  def login(loginSpec: LoginSpec) = {
    val users = usersCollection.find(and(equal("username", loginSpec.username), equal("passwordHash", loginSpec.passwordHash))).first().results()

    if (!users.isEmpty)
      Some(users(0))
    else
      None
  }

  def logout(token: String) = {
    if(!isTokenBlacklisted(token))
      tokenCollection.insertOne(Token(new ObjectId().toString, token)).results()
  }

  // create user
  def createUser(userSpec: UserSpec) = {
    val newUser = User(userSpec)

    usersCollection.insertOne(newUser).results()
    usersCollection.find(equal("username", userSpec.username)).first().results()(0)
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

  def addContract(username: String, contract: BotContract) = {
    var contracts = getByUsername(username).botContracts

    if(!contracts.contains(contract)) {

      contracts :+= contract
      val contractsBson = BsonArray()

      contracts.foreach(
        el => {
          val contractBson = BsonDocument()
          contractBson.append("botName", BsonString(el.botName))
          contractBson.append("profitMargin", BsonNumber(el.profitMargin))

          contractsBson.add(contractBson)
        }
      )

      val botContracts= BsonDocument()
      val javaContracts = new util.ArrayList[BotContract]
      contracts.foreach{javaContracts.add(_)}
      botContracts.append("botContracts", contractsBson)
      val updateQuery = new BsonDocument()
      updateQuery.append("$set", botContracts)
      usersCollection.findOneAndUpdate(equal("username", username), updateQuery).results()
      usersCollection.find(equal("username", username)).results()(0)
    }
  }
}
