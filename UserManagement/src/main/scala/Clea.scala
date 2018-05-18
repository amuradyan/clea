import java.util

import accounting.{Accounting, BookRecord, DepositWithdrawSpec, RecordSearchCriteria}
import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.headers.{`Access-Control-Allow-Credentials`, `Access-Control-Allow-Headers`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Origin`, _}
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import akka.stream.ActorMaterializer
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import token_management.{JWTPayload, LoginSpec, TokenManagement}
import user_management._
import helpers.CorsSupport

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import contracts.{BotContractSpec, Contracts}
import mailer.Mailer

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
                val loginSpec = new Gson().fromJson(loginSpecJson, classOf[LoginSpec])
                val user = UserManagement.login(loginSpec)

                user match {
                  case Some(token) => {
                    logger.info(s"${loginSpec.username} logged in")
                    complete(token)
                  }
                  case None =>
                    complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity("Invalid username/password")))
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
              complete(s"${payload.sub} logged out")
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
                        complete(new Gson().toJson(UserExposed(newUser.username)))
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

                    parameters('users.as[List[String]].?, 'regions.as[List[String]].?) {
                      (users, regions) => {
                        var allUsers = Seq[UserExposed]()
                        payload.role match {
                          case "admin" =>
                            allUsers = UserManagement.getUsers(UserSearchCriteria(users, regions)).map(UserExposed(_))
                          case "manager" => {
                            val requestor = UserManagement.getByUsername(payload.sub)
                            allUsers = UserManagement.getUsers(UserSearchCriteria(users, Some(List(requestor.region)))).map(UserExposed(_))
                          }
                          case "client" => {
                            val requestor = UserManagement.getByUsername(payload.sub)
                            allUsers = UserManagement.getUsers(UserSearchCriteria(Some(List(requestor.username)), None)).map(UserExposed(_))
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
                      complete(new Gson().toJson(UserExposed(jwtPayload.sub)))
                    }
                  }
                }
              } ~
              pathPrefix(Segment) {
                username => {
                  val user = UserManagement.getByUsername(username)
                  pathEnd {
                    corsHandler {
                      get {
                        logger.info(s"User $username accessed by ${payload.role}")
                        payload.role match {
                          case "admin" => {
                            complete(new Gson().toJson(UserExposed(username)))
                          }
                          case "manager" => {
                            val manager = UserManagement.getByUsername(payload.sub)

                            if (manager.region.equalsIgnoreCase(user.region))
                              complete(new Gson().toJson(UserExposed(username)))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                          case "client" => {
                            if (payload.sub.equalsIgnoreCase(username))
                              complete(new Gson().toJson(UserExposed(username)))
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
                                UserManagement.updateUser(username, new Gson().fromJson(userUpdateSpecJson, classOf[UserUpdateSpec]))
                                complete(new Gson().toJson(UserExposed(username)))
                              } else
                                complete(HttpResponse(StatusCodes.OK))
                            }
                          }
                        }
                      }
                  } ~
                    pathPrefix("password") {
                      corsHandler {
                        patch {
                          complete("Password reset request sent")
                        }
                      }
                    } ~
                    pathPrefix("contracts") {
                      corsHandler {
                        post {
                          entity(as[String]) {
                            botContractSpecJson => {
                              val botContractSpec = new Gson().fromJson(botContractSpecJson, classOf[BotContractSpec])

                              payload.role match {
                                case "admin" => {
                                  Contracts.createContract(username, botContractSpec)
                                  Mailer.sendAddContractReply(username, botContractSpec)
                                  complete(s"Deposit request by $username. A record was also added")
                                }
                                case "client" => {
                                  Mailer.sendAddContractRequest(username, botContractSpec)
                                  complete(s"Deposit request by $username")
                                }
                                case _ => complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                            }
                          }
                        }
                      } ~
                        corsHandler {
                          get {
                            payload.role match {
                              case "manager" => {
                                if (payload.region.equals(user.region)) {
                                  val contracts = Contracts.getContractsOf(username)
                                  complete(new Gson().toJson(contracts))
                                } else
                                  complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                              case "client" => {
                                if (payload.sub.equals(username)) {
                                  val contracts = Contracts.getContractsOf(username)
                                  complete(new Gson().toJson(contracts))
                                } else
                                  complete(HttpResponse(StatusCodes.Unauthorized))
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
                                parameters('recordSearchCriteria.as[String].?) {
                                  recordSearchCriteriaJson => {
                                    if (payload.sub.equals(username) || payload.role.equals("admin") ||
                                      (payload.role.equals("manager") && payload.region.equals(user.region))) {
                                      var recordSearchCriteria = RecordSearchCriteria()

                                      recordSearchCriteriaJson match {
                                        case Some(criteria) =>
                                          recordSearchCriteria = new Gson().fromJson(recordSearchCriteriaJson.get, classOf[RecordSearchCriteria])
                                        case None => ;
                                      }

                                      recordSearchCriteria.bookNames = Some(List(bookId))
                                      recordSearchCriteria.userIds = Some(List(username))

                                      val records = Accounting.getRecords(recordSearchCriteria)
                                      complete(new Gson().toJson(records))
                                    } else {
                                      complete(HttpResponse(StatusCodes.Unauthorized))
                                    }
                                  }
                                }
                              }
                            } ~
                              corsHandler {
                                post {
                                  entity(as[String]) {
                                    DWSpecJson => {
                                      val DWSpec = new Gson().fromJson(DWSpecJson, classOf[DepositWithdrawSpec])

                                      payload.role match {
                                        case "admin" => {
                                          val balance = Accounting.getBalance(bookId)
                                          val record = BookRecord(username, bookId, System.currentTimeMillis(), DWSpec.`type`, DWSpec.source, DWSpec.amount, DWSpec.fee, balance)
                                          val book = Accounting.addRecord(bookId, record)
                                          Mailer.sendDWReply(username, DWSpec)
                                          complete(new Gson().toJson(book))
                                        }
                                        case "client" => {
                                          Mailer.sendDWRequest(username, DWSpec)
                                          complete(s"${DWSpec.`type`} request sent")
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                          } ~
                            pathEnd {
                              corsHandler {
                                get {
                                  if (payload.sub.equals(username) || payload.role.equals("admin") ||
                                    (payload.role.equals("manager") && payload.region.equals(user.region)))
                                  {
                                    val book = Accounting.getBook(bookId)
                                    book match {
                                    case Some(b) => complete(new Gson().toJson(book))
                                    case None => complete(HttpResponse(StatusCodes.NotFound))
                                  }
                                  } else
                                    complete(HttpResponse(StatusCodes.Unauthorized))
                                }
                              }
                            }
                        }
                      } ~
                        pathEnd {
                          corsHandler {
                            get {
                              if (payload.role.equals("admin")) {
                                val bookBriefs = Accounting.getBooks(username)
                                complete(new Gson().toJson(bookBriefs))
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
            pathEnd {
              corsHandler {
                get {
                  parameters('recordSearchCriteria.as[String].?) {
                    recordSearchCriteriaJson => {
                      var recordSearchCriteria = RecordSearchCriteria()
                      val requestor = UserManagement.getByUsername(payload.sub)

                      recordSearchCriteriaJson match {
                        case Some(criteria) => {
                          recordSearchCriteria = new Gson().fromJson(recordSearchCriteriaJson.get, classOf[RecordSearchCriteria])
                        }
                        case None => ;
                      }

                      payload.sub match {
                        case "manager" => recordSearchCriteria.region = Some(requestor.region)
                        case "client" => recordSearchCriteria.userIds = Some(List(payload.sub))
                        case _ => ;
                      }

                      val records = Accounting.getRecords(recordSearchCriteria)
                      complete(new Gson().toJson(records))
                    }
                  }
                }
              }
            } ~
            pathPrefix("records") {
              parameters('recordSearchCriteria.as[String].?) {
                recordSearchCriteriaJson => {
                  var recordSearchCriteria = RecordSearchCriteria()
                  val requestor = UserManagement.getByUsername(payload.sub)

                  recordSearchCriteriaJson match {
                    case Some(criteria) => {
                      recordSearchCriteria = new Gson().fromJson(recordSearchCriteriaJson.get, classOf[RecordSearchCriteria])
                    }
                    case None => ;
                  }

                  payload.sub match {
                    case "manager" => recordSearchCriteria.region = Some(requestor.region)
                    case "client" => recordSearchCriteria.userIds = Some(List(payload.sub))
                    case _ => ;
                  }

                  val records = Accounting.getRecords(recordSearchCriteria)
                  val recordsJava = new util.ArrayList[BookRecord]()
                  records foreach {e => recordsJava.add(e)};
                  logger.info(s"${records.length} records found")

                  complete(new Gson().toJson(recordsJava))
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
