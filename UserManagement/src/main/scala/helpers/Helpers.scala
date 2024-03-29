package helpers

import java.util.concurrent.TimeUnit

import accounting.{Book, BookRecord}
import adapters.alpinist.adapter.AlpinistRecordPointer
import com.google.gson.Gson
import contracts.BotContract
import org.mongodb.scala._
import token_management.Token
import user_management.User

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

  implicit class TokenObservable[C](val observable: Observable[Token]) extends ImplicitObservable[Token] {
    val gson = new Gson();
    override val converter: (Token) => String = (doc) => gson.toJson(doc)
  }

  implicit class BookObservable[C](val observable: Observable[Book]) extends ImplicitObservable[Book] {
    val gson = new Gson();
    override val converter: (Book) => String = (doc) => gson.toJson(doc)
  }

  implicit class BotContractObservable[C](val observable: Observable[BotContract]) extends ImplicitObservable[BotContract] {
    val gson = new Gson();
    override val converter: (BotContract) => String = (doc) => gson.toJson(doc)
  }

  implicit class BookRecordObservable[C](val observable: Observable[BookRecord]) extends ImplicitObservable[BookRecord] {
    val gson = new Gson();
    override val converter: (BookRecord) => String = (doc) => gson.toJson(doc)
  }

  implicit class AlpinistRecordPointerObservable[C](val observable: Observable[AlpinistRecordPointer]) extends ImplicitObservable[AlpinistRecordPointer] {
    val gson = new Gson();
    override val converter: (AlpinistRecordPointer) => String = (doc) => gson.toJson(doc)
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
