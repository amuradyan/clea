package token_management

import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import helpers.Helpers._
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters._
import org.quartz.impl.StdSchedulerFactory
import org.quartz.{Job, JobExecutionContext}
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim, JwtHeader}
import user_management.User

/**
  * Created by spectrum on 5/14/2018.
  */

case class Token(_id: String, token: String)
case class LoginSpec(username: String, passwordHash: String) {
  def isValid = username != null && username.nonEmpty && passwordHash != null && passwordHash.nonEmpty
}
case class JWTPayload(iat: Long, exp: Long, sub: String, role: String, region: String)

class TokenCleanup extends Job {
  val tokenCollection = CleaMongoClient.getTokenCollection

  override def execute(context: JobExecutionContext) = {
    tokenCollection.drop().results()
  }
}


class TokenManagement
object TokenManagement {

  val conf = ConfigFactory.load()
  val secret_key = conf.getString("secret_key")
  val scheduler = StdSchedulerFactory.getDefaultScheduler()
  val tokenCollection = CleaMongoClient.getTokenCollection

  def setup {
    import org.quartz.JobBuilder._
    import org.quartz.SimpleScheduleBuilder._
    import org.quartz.TriggerBuilder._

    val job = newJob(classOf[TokenCleanup]).withIdentity("token-cleanup", "cleanup").build()

    val trigger = newTrigger.withIdentity("token-cleanup-trigger", "cleanup-triggers")
      .startNow()
      .withSchedule(simpleSchedule()
        .withIntervalInHours(12)
        .repeatForever())
      .build()
    scheduler.scheduleJob(job, trigger)

  }

  def issueToken(user: User) = {
    val header = JwtHeader(JwtAlgorithm.HS512, "JWT")

    var claim = JwtClaim()
    claim = claim +("iat", System.currentTimeMillis())
    claim = claim +("exp", System.currentTimeMillis() + 86400)
    claim = claim +("sub", user.username)
    claim = claim +("role", user.role)
    claim = claim +("region", user.region)

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
