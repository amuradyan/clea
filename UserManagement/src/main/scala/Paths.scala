import java.util

import Clea._
import accounting._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpOrigin
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.Segment
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
import com.google.gson.Gson
import contracts.{BotContract, BotContractSpec, Contracts}
import mailer.TalisantMailer
import reports.RepGen
import token_management.{JWTPayload, LoginSpec, TokenManagement}
import user_management.{PasswordResetSpec, _}

import scala.concurrent.Future

/**
  * Created by spectrum on 6/10/2018.
  */
trait CsvParameters {
  implicit def csvSeqParamMarshaller: FromStringUnmarshaller[Seq[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",")))

  implicit def csvListParamMarshaller: FromStringUnmarshaller[List[String]] =
    Unmarshaller(ex => s => Future.successful(s.split(",").toList))
}

final object CsvParameters extends CsvParameters

trait Paths {
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

                    if (!loginSpec.isValid)
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
          if (rc.request.method.equals(HttpMethods.OPTIONS)) {
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

                        if (!userSpec.isValid)
                          complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid user spec"))

                        val newUser = UserManagement.createUser(userSpec)

                        newUser match {
                          case Some(u) => {
                            val res = new Gson().toJson(UserExposed(u.username))
                            complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                          }
                          case None => complete(HttpResponse(StatusCodes.NotFound))
                        }
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
                            requestor match {
                              case Some(u) => allUsers = UserManagement.getUsers(UserSearchCriteria(users, Some(List(u.region)))).map(UserExposed(_))
                              case None => complete(HttpResponse(StatusCodes.NotFound))
                            }
                          }
                          case "client" => {
                            val requestor = UserManagement.getByUsername(payload.sub)

                            requestor match {
                              case Some(u) => allUsers = UserManagement.getUsers(UserSearchCriteria(Some(List(u.username)), None)).map(UserExposed(_))
                              case None => complete(HttpResponse(StatusCodes.NotFound))
                            }
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

                            manager match {
                              case Some(m) => {
                                if (m.region.equalsIgnoreCase(m.region))
                                  res = new Gson().toJson(UserExposed(username))
                                else
                                  complete(HttpResponse(StatusCodes.Unauthorized))
                              }
                              case None => complete(HttpResponse(StatusCodes.NotFound))
                            }
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
                              if (!userUpdateSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid user update spec"))

                              if (payload.sub.equalsIgnoreCase("admin")) {
                                if (UserManagement.updateUser(username, userUpdateSpec) != Nil) {
                                  val user = UserExposed(username)
                                  val res = new Gson().toJson(user)
                                  TalisantMailer.sendPersonalDataChangeRequestApprovalFromSupportToClient(user, userUpdateSpec)
                                  complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                } else {
                                  complete(HttpResponse(status = StatusCodes.InternalServerError, entity = s"Unable to update user ${username}"))
                                }
                              } else {
                                TalisantMailer.sendPersonalDataChangeRequestFromCleaToSupport(UserExposed(username), userUpdateSpec)
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

                              if (!passwordResetSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid password reset spec"))

                              payload.role match {
                                case "admin" => {
                                  UserManagement.changePassword(username, passwordResetSpec)
                                  TalisantMailer.sendPasswordResetRequestApprovalFromSupportToClient(UserExposed(username), passwordResetSpec)
                                  complete("Password reset")
                                }
                                case "client" => {
                                  TalisantMailer.sendPasswordResetRequestFromCleaToSupport(UserExposed(username), passwordResetSpec)
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

                              if (!botContractSpec.isValid)
                                complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid bot contract spec"))

                              payload.role match {
                                case "admin" => {
                                  Contracts.createContract(username, botContractSpec)
                                  complete(s"Deposit request by $username. A record was also added")
                                }
                                case "client" => {
                                  complete(HttpResponse(status = StatusCodes.InternalServerError, entity = "Not implemented"))
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
                                user match {
                                  case Some(u) => {
                                    if (payload.region.equals(u.region))
                                      contracts = Contracts.getContractsOf(username)
                                    else
                                      complete(HttpResponse(StatusCodes.Unauthorized))
                                  }
                                  case None => complete(HttpResponse(StatusCodes.NotFound))
                                }
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
                                import CsvParameters._

                                parameters(
                                  'books.as[List[String]].?,
                                  'dateFrom.as[Long].?,
                                  'dateTo.as[Long].?,
                                  'userIDs.as[List[String]].?,
                                  'regions.as[List[String]].?,
                                  'sources.as[List[String]].?) {
                                  (books, dateFrom, dateTo, userIDs, regions, sources) => {

                                    user match {
                                      case Some(u) => {
                                        if (payload.sub.equals(username) || payload.role.equals("admin") ||
                                          (payload.role.equals("manager") && payload.region.equals(u.region))) {
                                          val recordSearchCriteria = RecordSearchCriteria(books, dateFrom, dateTo, userIDs, regions, sources)

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
                                      case None => complete(HttpResponse(StatusCodes.NotFound))
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

                                      if (!DWSpec.isValid)
                                        complete(HttpResponse(status = StatusCodes.BadRequest, entity = "Invalid deposit/withdraw spec"))

                                      payload.role match {
                                        case "admin" => {
                                          val record = BookRecord(username, bookId, System.currentTimeMillis(), DWSpec.`type`, DWSpec.source, DWSpec.amount, DWSpec.fee)
                                          val book = Accounting.addRecord(bookId, record)
                                          TalisantMailer.sendDWRequestApprovalFromSupportToClient(UserExposed(username), DWSpec)
                                          val res = new Gson().toJson(book)
                                          complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                        }
                                        case "client" => {
                                          TalisantMailer.sendDWRequestFromCleaToSupport(UserExposed(username), DWSpec)
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

                                  user match {
                                    case Some(u) => {
                                      if (payload.sub.equals(username) || payload.role.equals("admin") ||
                                        (payload.role.equals("manager") && payload.region.equals(u.region))) {

                                        val book = Accounting.getBook(username, bookId)
                                        book match {
                                          case Some(b) => {
                                            val res = new Gson().toJson(b)
                                            complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                                          }
                                          case None => complete(HttpResponse(StatusCodes.NotFound))
                                        }
                                      } else
                                        complete(HttpResponse(StatusCodes.Unauthorized))
                                    }
                                    case None => complete(HttpResponse(StatusCodes.NotFound))
                                  }
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
                import CsvParameters._

                parameters(
                  'books.as[List[String]].?,
                  'dateFrom.as[Long].?,
                  'dateTo.as[Long].?,
                  'userIDs.as[List[String]].?,
                  'regions.as[List[String]].?,
                  'sources.as[List[String]].?) {
                  (books, dateFrom, dateTo, userIDs, regions, sources) => {
                    val recordSearchCriteria = RecordSearchCriteria(books, dateFrom, dateTo, userIDs, regions, sources)
                    val requestor = UserManagement.getByUsername(payload.sub)

                    requestor match {
                      case Some(u) => {
                        payload.role match {
                          case "manager" => recordSearchCriteria.region = Some(List(u.region))
                          case "client" => recordSearchCriteria.userIds = Some(List(payload.sub))
                          case _ => ;
                        }

                        val records = Accounting.getRecords(recordSearchCriteria)
                        val recordsJ = new util.ArrayList[BookRecordExposed]()
                        records foreach { e => recordsJ.add(e) }
                        val res = new Gson().toJson(recordsJ)
                        complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, res)))
                      }
                      case None => complete(HttpResponse(StatusCodes.NotFound))
                    }
                  }
                }
              }
            }
          } ~
          pathPrefix("reports") {
            corsHandler(origin) {
              import CsvParameters._

              parameters(
                'books.as[List[String]].?,
                'dateFrom.as[Long].?,
                'dateTo.as[Long].?,
                'userIDs.as[List[String]].?,
                'regions.as[List[String]].?,
                'sources.as[List[String]].?,
                'format.as[String] ? "xlsx") {
                (books, dateFrom, dateTo, userIDs, regions, sources, format) => {
                  val recordSearchCriteria = RecordSearchCriteria(books, dateFrom, dateTo, userIDs, regions, sources)
                  val requestor = UserManagement.getByUsername(payload.sub)

                  requestor match {
                    case Some(u) => {
                      payload.role match {
                        case "manager" => recordSearchCriteria.region = Some(List(u.region))
                        case "client" => recordSearchCriteria.userIds = Some(List(payload.sub))
                        case _ => ;
                      }

                      val records = Accounting.getRecords(recordSearchCriteria)
                      RepGen.generate(records, format)

                      complete(HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, "")))
                    }
                    case None => complete(HttpResponse(StatusCodes.NotFound))
                  }
                }
              }
            }
          }
      }
  }
}

