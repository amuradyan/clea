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

class Clea

object Clea extends App with CorsSupport with Paths {
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
