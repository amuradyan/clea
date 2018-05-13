package token_management

import com.google.gson.Gson
import com.mongodb.ConnectionString
import com.typesafe.config.ConfigFactory
import helpers.Helpers
import org.bson.types.ObjectId
import org.mongodb.scala.{MongoClient, MongoClientSettings, MongoCollection, MongoCredential}
import org.mongodb.scala.connection.ClusterSettings
import org.mongodb.scala.model.Filters._
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim, JwtHeader}
import user_management.User

/**
  * Created by spectrum on 5/14/2018.
  */

case class Token(_id: String, token: String)
case class LoginSpec(username: String, passwordHash: String)
case class JWTPayload(iat: Long, exp: Long, sub: String, role: String)

class TokenManagement
object TokenManagement {
  import Helpers._
  import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
  import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
  import org.mongodb.scala.bson.codecs.Macros._

  val codecRegistry = fromRegistries(fromProviders(classOf[Token]), DEFAULT_CODEC_REGISTRY)

  val secret_key = conf.getString("secret_key")

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

  val tokenCollection: MongoCollection[Token] = cleaDB.getCollection("tokens")

  def issueToken(user: User) = {
    val header = JwtHeader(JwtAlgorithm.HS512, "JWT")

    var claim = JwtClaim()
    claim = claim +("iat", System.currentTimeMillis())
    claim = claim +("exp", System.currentTimeMillis() + 86400)
    claim = claim +("sub", user.username)
    claim = claim +("role", user.role)

    Jwt.encode(header, claim, secret_key)
  }

  def isTokenBlacklisted(token: String) = {
    val tokens = tokenCollection.find(equal("token", token)).first().results()

    !tokens.isEmpty
  }

  def blacklistToken(token: String) = {
    if(!isTokenBlacklisted(token))
      tokenCollection.insertOne(Token(new ObjectId().toString, token)).results()
  }

  def isValid(token: String): Boolean = {
    Jwt.isValid(token, secret_key, Seq(JwtAlgorithm.HS512))
  }

  def decode(token: String): JWTPayload =
    new Gson().fromJson(Jwt.decode(token, secret_key, Seq(JwtAlgorithm.HS512)).get, classOf[JWTPayload])

}
