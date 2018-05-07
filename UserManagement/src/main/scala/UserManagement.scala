import java.util.concurrent.TimeUnit

import com.google.gson.Gson
import com.mongodb.ConnectionString
import com.typesafe.config.ConfigFactory
import org.bson.conversions.Bson
import org.bson.types.ObjectId
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonString, conversions}
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

object UserManagement {

  import Helpers._
  import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
  import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
  import org.mongodb.scala.bson.codecs.Macros._

  val codecRegistry = fromRegistries(fromProviders(classOf[User]), DEFAULT_CODEC_REGISTRY)

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


  var blacklistedTokens = Seq[String]()

  val admin = new User(new ObjectId().toString, "Admin", "Admin", "admin", "admin@talisant.com", "+37493223775",
    "ARM", "admin", "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC", new ObjectId().toString)


  // Methods

  def setup {
    val admins = usersCollection.find(equal("username", "admin")).first().results()

    if (admins.isEmpty) {
      usersCollection.insertOne(admin).results()
    }
  }

  def isTokenBlacklisted(token: String) = blacklistedTokens.contains(token)

  // login
  def login(loginSpec: LoginSpec) = {
    val users = usersCollection.find(and(equal("username", loginSpec.username), equal("passwordHash", loginSpec.passwordHash))).first().results()

    if (!users.isEmpty)
      Some(users(0))
    else
      None
  }

  def logout(token: String) = {
    blacklistedTokens :+= token
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
  def updateUser(userSpec: UserSpec) = {
    usersCollection.replaceOne(equal("username", userSpec.username), User(userSpec)).results()(0)
    usersCollection.find(equal("username", userSpec.username)).results()(0)
  }

  def getUsers(userSearchCriteria: UserSearchCriteria) = {
    var userIdsFilter, regionFilter: Bson = null
    val filter = BsonDocument()

    if (!userSearchCriteria.userIds.isEmpty) {
      val inFilter = BsonDocument()
      inFilter.append("$in", BsonArray(userSearchCriteria.userIds))
      filter.append("username", inFilter)
    }

    if (!userSearchCriteria.region.isEmpty)
      filter.append("region", BsonString(userSearchCriteria.region))

    usersCollection.find(filter).results()
  }
}
