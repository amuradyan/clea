import java.util

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.headers.{HttpOrigin, HttpOriginRange}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import org.mongodb.scala.bson.BsonDocument
import token_management.{JWTPayload, LoginSpec, TokenManagement}
import user_management._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
//import scala.collection.JavaConverters._

/**
  * Created by spectrum on 5/2/2018.
  */

case class BookRecord(userId: String,
                      date: Long,
                      recordType: String,
                      amount: Float,
                      fee: Float,
                      source: String,
                      currentTotalBalance: Float,
                      bookId: String)


trait CsvParameters {
  implicit def csvSeqParamMarshaller: FromStringUnmarshaller[Seq[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",")))

  implicit def csvListParamMarshaller: FromStringUnmarshaller[List[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",").toList))
}

final object CsvParameters extends CsvParameters

case class BookRecordSearchCriteria(dateFrom: Option[Long] = None,
                                    dateTo: Option[Long] = None,
                                    userIds: Option[List[String]] = None,
                                    region: Option[String] = None)

class Clea

object Clea {
  val logger = Logger[Clea]

  val conf = ConfigFactory.load()
  val API_KEY = conf.getString("api_key")

  val host = conf.getString("app.host")
  val port = conf.getInt("app.port")

  def main(args: Array[String]) {
    implicit val actorSystem = ActorSystem("Clea")
    implicit val materializer = ActorMaterializer()

    implicit val executionCtx = actorSystem.dispatcher

    UserManagement.setup
    import ch.megard.akka.http.cors.scaladsl.CorsDirectives._

    val settings = CorsSettings.defaultSettings.withAllowedOrigins(HttpOriginRange(HttpOrigin("http://andranik.am")))
    val route = cors(settings) {
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
                  case Some(token) => complete(token)
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

            logger.info(s"user_management.Token: $token")
            logger.info(s"Is blacklisted: ${TokenManagement.isTokenBlacklisted(token)}")
            logger.info(s"Is valid: ${TokenManagement.isValid(token)}")

            payload = TokenManagement.decode(token)
            !TokenManagement.isTokenBlacklisted(token) && TokenManagement.isValid(token)
          } else false
        }) {
          pathPrefix("token") {
            cors(settings){
              delete {
                UserManagement.logout(token)
                complete("Commencing logout")
              }
            }
          } ~
            pathPrefix("users") {
              pathEnd {
                post {
                  entity(as[String]) {
                    userSpecJson => {
                      if (payload.role.equalsIgnoreCase("admin")) {
                        logger.info(userSpecJson)
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
                        var allUsers = Seq[UserExposed]()
                        payload.role match {
                          case "admin" =>
                            allUsers = UserManagement.getUsers(UserSearchCriteria(users, region)).map(UserExposed(_))
                          case "manager" => {
                            val user = UserManagement.getByUsername(payload.sub)
                            allUsers = UserManagement.getUsers(UserSearchCriteria(users, Some(user.region))).map(UserExposed(_))
                          }
                          case "client" => {
                            val user = UserManagement.getByUsername(payload.sub)
                            allUsers = UserManagement.getUsers(UserSearchCriteria(Some(List(user.username)), None)).map(UserExposed(_))
                          }
                        }


                        complete(new Gson().toJson(allUsers.toArray))
                      }
                    }
                  }
              } ~
                pathPrefix("me") {
                  pathEnd {
                    get {
                      val jwtPayload = TokenManagement.decode(token)
                      val user = UserManagement.getByUsername(jwtPayload.sub)
                      complete(new Gson().toJson(UserExposed(user)))
                    }
                  } ~
                    pathPrefix("password") {
                      patch {
                        complete("Password reset request sent")
                      }
                    }
                } ~
                pathPrefix(Segment) {
                  username => {
                    pathEnd {
                      get {
                        payload.role match {
                          case "admin" => {
                            logger.info(UserExposed(UserManagement.getByUsername(username)).toString)
                            complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                          }
                          case "manager" => {
                            val user = UserManagement.getByUsername(username)
                            val manager = UserManagement.getByUsername(payload.sub)

                            if (manager.region.equalsIgnoreCase(user.region))
                              complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                          case "client" => {
                            if (payload.sub.equalsIgnoreCase(username))
                              complete(new Gson().toJson(UserExposed(UserManagement.getByUsername(username))))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                        }
                      } ~
                        delete {
                          if (payload.role.equalsIgnoreCase("admin")) {
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
                    } ~
                    pathPrefix("withdraw") {
                      post {
                        complete(s"Withdrawal request by $username")
                      }
                    } ~
                    pathPrefix("deposit") {
                      post {
                        complete(s"Deposit request by $username")
                      }
                    } ~
                    pathPrefix("contracts") {
                      post {
                        entity(as[String]){
                          contractSpecJson => {
                            val contract = new Gson().fromJson(contractSpecJson, classOf[BotContract])

                            UserManagement.addContract(username, contract)
                            complete("")
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

    val bindingFuture: Future[ServerBinding] = null

    val f = for {bindingFuture <- Http().bindAndHandle(route, host, port)
                 waitOnFuture <- Promise[Done].future
    } yield waitOnFuture

    logger.info(s"Server online at http://$host:$port/")

    sys.addShutdownHook {
      bindingFuture
        .flatMap(_.unbind())
        .onComplete(_ => actorSystem.terminate())
    }

    Await.ready(f, Duration.Inf)
  }
}
