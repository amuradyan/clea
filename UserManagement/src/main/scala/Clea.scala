import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import akka.stream.ActorMaterializer
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import org.bson.types.ObjectId
import org.mongodb.scala.bson.BsonDocument
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim, JwtHeader}

import scala.concurrent.Future
import scala.io.StdIn

/**
  * Created by spectrum on 5/2/2018.
  */
object User {
  def apply(userSpec: UserSpec): User = new User(new ObjectId().toString, userSpec.name, userSpec.surname,
    userSpec.username, userSpec.email, userSpec.phone, userSpec.region, userSpec.role, userSpec.passwordHash,
    new ObjectId().toString)
}

case class User(_id: String,
                name: String,
                surname: String,
                username: String,
                email: String,
                phone: String,
                region: String,
                role: String,
                passwordHash: String,
                bookId: String)

case class UserSpec(name: String,
                    surname: String,
                    username: String,
                    email: String,
                    phone: String,
                    region: String,
                    role: String,
                    passwordHash: String)

object UserExposed {
  def apply(user: User): UserExposed = new UserExposed(user.name, user.surname, user.username, user.email, user.phone,
    user.region, user.role, user.bookId)
}

case class UserExposed(name: String,
                       surname: String,
                       username: String,
                       email: String,
                       phone: String,
                       region: String,
                       role: String,
                       bookId: String)

case class LoginSpec(username: String, passwordHash: String)

case class BookRecord(userId: String,
                      date: Long,
                      recordType: String,
                      amount: Float,
                      fee: Float,
                      source: String,
                      currentTotalBalance: Float,
                      bookId: String)

case class JWTPayload(iat: Long, exp: Long, sub: String, role: String)

trait CsvParameters {
  implicit def csvSeqParamMarshaller: FromStringUnmarshaller[Seq[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",")))

  implicit def csvListParamMarshaller: FromStringUnmarshaller[List[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",").toList))
}

final object CsvParameters extends CsvParameters

object UserSearchCriteria {
  def apply(usersIds: Option[List[String]], region: Option[String]) =
    new UserSearchCriteria(usersIds.getOrElse(List()), region.getOrElse(""))
}

case class UserSearchCriteria(userIds: List[String] = List(), region: String = "")

object BookRecordSearchCriteria {
  def apply(dateFrom: Option[Long], dateTo: Option[Long], usersIds: Option[List[String]], region: Option[String]) =
    new BookRecordSearchCriteria(dateFrom.getOrElse(-1), dateTo.getOrElse(-1), usersIds.getOrElse(List()), region.getOrElse(""))
}

case class BookRecordSearchCriteria(dateFrom: Long = -1, dateTo: Long = -1, userIds: List[String] = List(), region: String = "")


class Clea

object Clea {
  val logger = Logger[Clea]

  val conf = ConfigFactory.load()
  val API_KEY = conf.getString("api_key")
  val secret_key = conf.getString("secret_key")

  def generateJwt(user: User) = {
    val header = JwtHeader(JwtAlgorithm.HS512, "JWT")

    var claim = JwtClaim()
    claim = claim +("iat", System.currentTimeMillis())
    claim = claim +("exp", System.currentTimeMillis() + 86400)
    claim = claim +("sub", user.username)
    claim = claim +("role", user.role)

    Jwt.encode(header, claim, secret_key)
  }

  def main(args: Array[String]) {
    implicit val actorSystem = ActorSystem("Clea")
    implicit val materializer = ActorMaterializer()

    implicit val executionCtx = actorSystem.dispatcher

    UserManagement.setup

    val route = {
      var payload: JWTPayload = null
      var token = ""

      pathSingleSlash {
        complete("It's alive!!!")
      } ~
        pathPrefix("token") {
          pathEnd {
            post {
              entity(as[String]) { loginSpecJson => {
                logger.info("Commencing login")
                val loginSpec = new Gson().fromJson(loginSpecJson, classOf[LoginSpec])
                val user = UserManagement.login(loginSpec)

                user match {
                  case Some(user) => complete(generateJwt(user))
                  case None => complete("Invalid username/password")
                }
              }
              }
            }
          }
        } ~
        authorize(rc => {
          val authHeader = rc.request.getHeader("Authorization")

          logger.info("About to auth")
          if (authHeader.isPresent) {
            token = authHeader.get().value()

            logger.info(s"Token: $token")
            logger.info(s"Is blacklisted: ${UserManagement.isTokenBlacklisted(token)}")
            logger.info(s"Is valid: ${Jwt.isValid(token, secret_key, Seq(JwtAlgorithm.HS512))}")

            payload = new Gson().fromJson(Jwt.decode(token, secret_key, Seq(JwtAlgorithm.HS512)).get, classOf[JWTPayload])
            !UserManagement.isTokenBlacklisted(token) && Jwt.isValid(token, secret_key, Seq(JwtAlgorithm.HS512))
          } else false
        }) {
          pathPrefix("token") {
            delete {
              UserManagement.logout(token)
              complete("Commencing logout")
            }
          } ~
            pathPrefix("users") {
              pathEnd {
                post {
                  entity(as[String]) {
                    userSpecJson => {
                      if(payload.role.equalsIgnoreCase("admin")) {
                        val userSpec = new Gson().fromJson(userSpecJson, classOf[UserSpec])
                        val newUser = UserManagement.createUser(userSpec)
                        complete(new Gson().toJson(UserExposed(newUser)))
                      } else {
                        complete(HttpResponse(StatusCodes.Unauthorized))
                      }
                    }
                  }
                } ~
                  get {
                    import CsvParameters._

                    parameters('users.as[List[String]].?, 'region.as[String].?) {
                      (users, region) => {
                        val allUsers = UserManagement.getUsers(UserSearchCriteria(users, region)).map(UserExposed.apply(_))
                        complete(new Gson().toJson(allUsers.toArray))
                      }
                    }
                  }
              } ~
                pathPrefix("me") {
                  pathEnd {
                    get {
                      val jwtPayload = new Gson().fromJson(Jwt.decode(token, secret_key, Seq(JwtAlgorithm.HS512)).get, classOf[JWTPayload])
                      val user = UserManagement.getByUsername(jwtPayload.sub)
                      complete(new Gson().toJson(UserExposed(user)))
                    }
                  }
                } ~
                pathPrefix(Segment) {
                  username => {
                    pathEnd {
                      get {
                        payload.role match {
                          case "admin" => {
                            complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                          }
                          case "manager" => {
                            val user = UserManagement.getByUsername(username)
                            val manager = UserManagement.getByUsername(payload.sub)

                            if(manager.region.equalsIgnoreCase(user.region))
                              complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                          case _ => {
                            if(payload.sub.equalsIgnoreCase(username))
                              complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                        }
                      } ~
                        delete {
                          if(payload.role.equalsIgnoreCase("admin")) {
                            UserManagement.deleteUser(username)
                            complete(s"User $username deleted")
                          } else
                            complete(HttpResponse(StatusCodes.Unauthorized))
                        } ~
                        patch {
                          entity(as[String]) {
                            userUpdateSpecJson => {
                              if (payload.sub.equalsIgnoreCase("admin")) {
                                val userUpdateDoc = BsonDocument(userUpdateSpecJson)
                                complete(new Gson().toJson(UserExposed(UserManagement.updateUser(username, userUpdateDoc))))
                              } else
                                complete(HttpResponse(StatusCodes.Unauthorized))
                            }
                          }
                        }
                    }
                  }
                }
            } ~
            pathPrefix("books") {
              pathPrefix(Segment) {
                bookId => {
                  get {
                    complete(s"Retrieved book $bookId")
                  } ~
                    patch {
                      entity(as[String]) {
                        bookRecord => {
                          complete(s"Edited book $bookId. Added record $bookRecord")
                        }
                      }
                    } ~
                    delete {
                      complete(s"Deleted book $bookId")
                    }
                }
              } ~
                pathEnd {
                  get {
                    complete("All books fetched")
                  } ~
                    post {
                      complete("Created a new book")
                    } ~
                    delete {
                      complete("Deleted all books")
                    }
                }
            }
        }
    }
    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

    logger.info(s"Server online at http://0.0.0.0:8080/\nPress RETURN to stop...")
    StdIn.readLine

    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => actorSystem.terminate())
  }
}
