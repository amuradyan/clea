import accounting.{Accounting, BookRecord, DepositWithdrawSpec, RecordSearchCriteria}
import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.headers.{`Access-Control-Allow-Credentials`, `Access-Control-Allow-Headers`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Origin`, _}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import akka.stream.ActorMaterializer
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import org.mongodb.scala.bson.BsonDocument
import token_management.{JWTPayload, LoginSpec, TokenManagement}
import user_management._
import helpers.CorsSupport

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import java.util

/**
  * Created by spectrum on 5/2/2018.
  */

trait CsvParameters {
  implicit def csvSeqParamMarshaller: FromStringUnmarshaller[Seq[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",")))

  implicit def csvListParamMarshaller: FromStringUnmarshaller[List[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",").toList))
}

final object CsvParameters extends CsvParameters

class Clea

object Clea extends App with CorsSupport {
  val logger = Logger[Clea]

  val conf = ConfigFactory.load()
  val API_KEY = conf.getString("api_key")

  val host = conf.getString("app.host")
  val port = conf.getInt("app.port")

  implicit val actorSystem = ActorSystem("Clea")
  implicit val executionCtx = actorSystem.dispatcher
  implicit val materializer = ActorMaterializer()

  UserManagement.setup

  val route = {
    var payload: JWTPayload = null
    var token = ""

    pathSingleSlash {
      complete("It's alive!!!")
    } ~
      corsHandler {
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
          corsHandler {
            delete {
              UserManagement.logout(token)
              complete("Commencing logout")
            }
          }
        } ~
          pathPrefix("users") {
            pathEnd {
              corsHandler {
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
                }
              } ~
                corsHandler {
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
                }
            } ~
              pathPrefix("me") {
                pathEnd {
                  corsHandler {
                    get {
                      val jwtPayload = TokenManagement.decode(token)
                      val user = UserManagement.getByUsername(jwtPayload.sub)
                      complete(new Gson().toJson(UserExposed(user)))
                    }
                  }
                } ~
                  pathPrefix("password") {
                    corsHandler {
                      patch {
                        complete("Password reset request sent")
                      }
                    }
                  }
              } ~
              pathPrefix(Segment) {
                username => {
                  pathEnd {
                    corsHandler {
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
                      }
                    } ~
                      corsHandler {
                        delete {
                          if (payload.role.equalsIgnoreCase("admin")) {
                            UserManagement.deleteUser(username)
                            complete(s"User $username deleted")
                          } else
                            complete(HttpResponse(StatusCodes.Unauthorized))
                        }
                      } ~
                      corsHandler {
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
                  } ~
                    pathPrefix("contracts") {
                      corsHandler {
                        post {
                          entity(as[String]) {
                            botContractSpec => {
                              payload.role match {
                                case "admin" => complete(s"Deposit request by $username. A record was also added")
                                case "client" => complete(s"Deposit request by $username")
                                case _ => complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                            }
                          }
                        }
                      }
                    } ~
                    pathPrefix("books") {
                      pathPrefix(Segment) {
                        bookId => {
                          pathPrefix("records") {
                            corsHandler {
                              get {
                                parameters('recordSearchCriteria.as[String]) {
                                  recordSearchCriteriaJson => {
                                    val recordSearchCriteria = new Gson().fromJson(recordSearchCriteriaJson, classOf[RecordSearchCriteria])
                                    val userId = UserManagement.getByUsername(username)
                                    recordSearchCriteria.userIds = Some(new util.ArrayList[String]() {
                                        userId
                                    })
                                    val bookName = Accounting.getBookBrief(bookId).getOrElse("")
                                    recordSearchCriteria.bookNames = Some(new util.ArrayList[String]() {
                                        bookName
                                    })

                                    val records = Accounting.getRecords(recordSearchCriteria)
                                    complete(new Gson().toJson(records))
                                  }
                                }
                              }
                            } ~
                              corsHandler {
                                patch {
                                  entity(as[String]) {
                                    DWSpecJson => {
                                      val DWSpec = new Gson().fromJson(DWSpecJson, classOf[DepositWithdrawSpec])
                                      val userId = UserManagement.getByUsername(username).name
                                      val balance = Accounting.getBalance(bookId)
                                      val record = BookRecord(userId, bookId, System.currentTimeMillis(), DWSpec.`type`, DWSpec.source, DWSpec.amount, DWSpec.fee, balance)
                                      Accounting.addRecord(bookId, record)
                                      complete("Done")
                                    }
                                  }
                                }
                              }
                          } ~
                            pathEnd {
                              corsHandler {
                                get {
                                  val bookBrief = Accounting.getBookBrief(bookId)
                                  complete(new Gson().toJson(bookBrief))
                                }
                              }
                            }
                        }
                      } ~
                        pathEnd {
                          corsHandler {
                            get {
                              val userId = UserManagement.getByUsername(username).name
                              val bookBriefs = Accounting.getBookBriefs(userId)
                              complete(new Gson().toJson(bookBriefs))
                            }
                          }
                        }
                    }

                }
              }
          } ~
          pathPrefix("books") {
            pathEnd {
              corsHandler {
                get {
                  parameters('recordSearchCriteria.as[String]) {
                    recordSearchCriteriaJson => {
                      val recordSearchCriteria = new Gson().fromJson(recordSearchCriteriaJson, classOf[RecordSearchCriteria])
                      val records = Accounting.getRecords(recordSearchCriteria)
                      complete(new Gson().toJson(records))
                    }
                  }
                }
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
