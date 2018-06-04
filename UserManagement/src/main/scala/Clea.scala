import java.io.InputStream
import java.security.{KeyStore, SecureRandom}
import java.util
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import accounting._
import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.headers.{HttpOrigin, Origin}
import akka.http.scaladsl.model.{HttpEntity, _}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{Unmarshaller, _}
import akka.stream.ActorMaterializer
import com.google.gson.Gson
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import token_management.{JWTPayload, LoginSpec, TokenManagement}
import user_management._
import helpers.{CorsSupport, Fetcher}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import contracts.{BotContract, BotContractSpec, Contracts}
import mailer.TalisantMailer

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

  val ks: KeyStore = KeyStore.getInstance("PKCS12")

  val keystore: InputStream = getClass.getResourceAsStream("/cert.p12")
  require(keystore != null, "Keystore required!")
  ks.load(keystore, "".toCharArray)

  val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
  keyManagerFactory.init(ks, "".toCharArray)

  val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
  tmf.init(ks)

  val sslContext: SSLContext = SSLContext.getInstance("TLS")
  sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)

  val https: HttpsConnectionContext = ConnectionContext.https(sslContext)

  Fetcher.setup
  UserManagement.setup
  TokenManagement.setup

  val route = {
    var payload: JWTPayload = null
    var origin = HttpOrigin("http://a.com")
    var token = ""

    extractRequestContext {
      rc => {
        val originHeader = rc.request.getHeader("Origin")

        if (originHeader.isPresent)
          origin = HttpOrigin(originHeader.get().value())

        pathSingleSlash {
          complete("It's alive!!!")
        } ~
          pathPrefix("token") {
            corsHandler(origin) {
              pathEnd {
                post {
                  entity(as[String]) { loginSpecJson => {
                    val loginSpec = new Gson().fromJson(loginSpecJson, classOf[LoginSpec])

                    if(!loginSpec.isValid)
                      complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity("Invalid username/password")))

                    val token = UserManagement.login(loginSpec)

                    token match {
                      case Some(t) => {
                        logger.info(s"${loginSpec.username} logged in")
                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, new Gson().toJson(t))))
                      }
                      case None =>
                        complete(HttpResponse(StatusCodes.NotFound, entity = HttpEntity("Invalid username/password")))
                    }
                  }
                  }
                }
              }
            }
          }
      }
    } ~
      authorize(rc => {
        val authHeader = rc.request.getHeader("Authorization")
        val originHeader = rc.request.getHeader("Origin")

        if (originHeader.isPresent)
          origin = HttpOrigin(originHeader.get().value())

        logger.info(s"---- Request from origin: ${origin.host} ---")

        logger.info("About to auth")
        if (authHeader.isPresent) {
          token = authHeader.get().value()

          logger.info(s"user_management.Token: $token")
          logger.info(s"Is blacklisted: ${TokenManagement.isTokenBlacklisted(token)}")
          logger.info(s"Is valid: ${TokenManagement.isValid(token)}")

          payload = TokenManagement.decode(token)
          !TokenManagement.isTokenBlacklisted(token) && TokenManagement.isValid(token)
        } else {
          if(rc.request.method.equals(HttpMethods.OPTIONS)){
            logger.error("Auth header not present but request type options")
            true
          } else {
            logger.error("Auth header not present")
            false
          }
        }
      }) {

        pathPrefix("token") {
          corsHandler(origin) {
            delete {
              UserManagement.logout(token)
              complete(s"${payload.sub} logged out")
            }
          }
        } ~
          pathPrefix("users") {
            pathEnd {
              corsHandler(origin) {
                post {
                  entity(as[String]) {
                    userSpecJson => {
                      if (payload.role.equalsIgnoreCase("admin")) {
                        logger.info(userSpecJson)
                        val userSpec = new Gson().fromJson(userSpecJson, classOf[UserSpec])

                        if(!userSpec.isValid)
                          complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid user spec"))

                        val newUser = UserManagement.createUser(userSpec)
                        val res = new Gson().toJson(UserExposed(newUser.username))
                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                      } else {
                        complete(HttpResponse(StatusCodes.Unauthorized))
                      }
                    }
                  }
                }
              } ~
                corsHandler(origin) {
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

                        val res = new Gson().toJson(allUsers.toArray)
                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                      }
                    }
                  }
                }
            } ~
            pathPrefix("me") {
              corsHandler(origin) {
                pathEnd {
                  get {
                    val jwtPayload = TokenManagement.decode(token)
                    val res = new Gson().toJson(UserExposed(jwtPayload.sub))
                    complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                  }
                }
              }
            } ~
              pathPrefix(Segment) {
                username => {
                  val user = UserManagement.getByUsername(username)
                  pathEnd {
                    corsHandler(origin) {
                      get {
                        logger.info(s"User $username accessed by ${payload.role}")
                        var res = ""
                        payload.role match {
                          case "admin" => {
                            res = new Gson().toJson(UserExposed(username))
                          }
                          case "manager" => {
                            val manager = UserManagement.getByUsername(payload.sub)

                            if (manager.region.equalsIgnoreCase(user.region))
                              res = new Gson().toJson(UserExposed(username))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                          case "client" => {
                            if (payload.sub.equalsIgnoreCase(username))
                              res = new Gson().toJson(UserExposed(username))
                            else
                              complete(HttpResponse(StatusCodes.Unauthorized))
                          }
                        }

                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                      }
                    } ~
                      corsHandler(origin) {
                        delete {
                          if (payload.role.equalsIgnoreCase("admin")) {
                            UserManagement.deleteUser(username)
                            complete(s"User $username deleted")
                          } else
                            complete(HttpResponse(StatusCodes.Unauthorized))
                        }
                      } ~
                      corsHandler(origin) {
                        patch {
                          entity(as[String]) {
                            userUpdateSpecJson => {
                              val userUpdateSpec = new Gson().fromJson(userUpdateSpecJson, classOf[UserUpdateSpec])
                              if(!userUpdateSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid user update spec"))

                              if (payload.sub.equalsIgnoreCase("admin")) {
                                UserManagement.updateUser(username, userUpdateSpec)
                                val res = new Gson().toJson(UserExposed(username))
                                TalisantMailer.sendDataChangeReply(username, userUpdateSpec)
                                complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))

                              } else {
                                TalisantMailer.sendDataChangeRequest(username, userUpdateSpec)
                                complete(HttpResponse(StatusCodes.OK))
                              }
                            }
                          }
                        }
                      }
                  } ~
                    pathPrefix("password") {
                      corsHandler(origin) {
                        patch {
                          entity(as[String]) {
                            passwordResetSpecJson => {
                              val passwordResetSpec = new Gson().fromJson(passwordResetSpecJson, classOf[PasswordResetSpec])

                              if(!passwordResetSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid password reset spec"))

                              payload.role match {
                                case "admin" => {
                                  UserManagement.changePassword(username, passwordResetSpec)
                                  TalisantMailer.sendPasswordResetReply(username, passwordResetSpec)
                                  complete("Password reset")
                                }
                                case "client" => {
                                  TalisantMailer.sendPasswordResetRequest(username, passwordResetSpec)
                                  complete("Password reset request sent")
                                }
                                case _ => complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                            }
                          }
                        }
                      }
                    } ~
                    pathPrefix("contracts") {
                      corsHandler(origin) {
                        post {
                          entity(as[String]) {
                            botContractSpecJson => {
                              val botContractSpec = new Gson().fromJson(botContractSpecJson, classOf[BotContractSpec])

                              if(!botContractSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid bot contract spec"))

                              payload.role match {
                                case "admin" => {
                                  Contracts.createContract(username, botContractSpec)
                                  TalisantMailer.sendAddContractReply(username, botContractSpec)
                                  complete(s"Deposit request by $username. A record was also added")
                                }
                                case "client" => {
                                  TalisantMailer.sendAddContractRequest(username, botContractSpec)
                                  complete(s"Deposit request by $username")
                                }
                                case _ => complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                            }
                          }
                        }
                      } ~
                        corsHandler(origin) {
                          get {
                            var contracts = new util.ArrayList[BotContract]()
                            payload.role match {
                              case "admin" => contracts = Contracts.getContractsOf(username)
                              case "manager" => {
                                if (payload.region.equals(user.region))
                                  contracts = Contracts.getContractsOf(username)
                                else
                                  complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                              case "client" => {
                                if (payload.sub.equals(username))
                                  contracts = Contracts.getContractsOf(username)
                                else
                                  complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                              case _ => complete(HttpResponse(StatusCodes.Unauthorized))
                            }

                            val res = new Gson().toJson(contracts)
                            complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                          }
                        }
                    } ~
                    pathPrefix("books") {
                      pathPrefix(Segment) {
                        bookId => {
                          pathPrefix("records") {
                            corsHandler(origin) {
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
                                      val recordsJ = new util.ArrayList[BookRecordExposed]()
                                      records foreach {
                                        recordsJ.add
                                      }
                                      val res = new Gson().toJson(recordsJ)
                                      complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                    } else {
                                      complete(HttpResponse(StatusCodes.Unauthorized))
                                    }
                                  }
                                }
                              }
                            } ~
                              corsHandler(origin) {
                                post {
                                  entity(as[String]) {
                                    DWSpecJson => {
                                      val DWSpec = new Gson().fromJson(DWSpecJson, classOf[DepositWithdrawSpec])

                                      if(!DWSpec.isValid)
                                        complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid deposit/withdraw spec"))

                                      payload.role match {
                                        case "admin" => {
                                          val record = BookRecord(username, bookId, System.currentTimeMillis(), DWSpec.`type`, DWSpec.source, DWSpec.amount, DWSpec.fee)
                                          val book = Accounting.addRecord(bookId, record)
                                          TalisantMailer.sendDWReply(username, DWSpec)
                                          val res = new Gson().toJson(book)
                                          complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                        }
                                        case "client" => {
                                          TalisantMailer.sendDWRequest(username, DWSpec)
                                          complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, "Done")))
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                          } ~
                            pathEnd {
                              corsHandler(origin) {
                                get {
                                  if (payload.sub.equals(username) || payload.role.equals("admin") ||
                                    (payload.role.equals("manager") && payload.region.equals(user.region))) {
                                    val book = Accounting.getBook(username, bookId)
                                    book match {
                                      case Some(b) => {
                                        val res = new Gson().toJson(book)
                                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                      }
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
                          corsHandler(origin) {
                            get {
                              if (payload.role.equals("admin")) {
                                val bookBriefs = Accounting.getBooks(username)
                                val res = new Gson().toJson(bookBriefs)
                                complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
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
            pathPrefix("records") {
              corsHandler(origin) {
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

                    payload.role match {
                      case "manager" => recordSearchCriteria.region = Some(requestor.region)
                      case "client" => recordSearchCriteria.userIds = Some(List(payload.sub))
                      case _ => ;
                    }

                    val records = Accounting.getRecords(recordSearchCriteria)
                    val recordsJ = new util.ArrayList[BookRecordExposed]()
                    records foreach { e => recordsJ.add(e) }
                    val res = new Gson().toJson(recordsJ)
                    complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
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

  logger.info(s"Server online at https://$host:$port/")

  sys.addShutdownHook {
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => actorSystem.terminate())
  }

  Await.ready(f, Duration.Inf)

  }
