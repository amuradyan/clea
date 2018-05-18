package accounting

import java.util

import adapters.cleversniper.CleversniperAdapter
import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonNumber, BsonString, conversions}
import user_management.{UserManagement, UserSearchCriteria}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by spectrum on 5/14/2018.
  */
case class DepositWithdrawSpec(`type`: String, bookId: String, source: String, amount: Float, fee: Float, note: String)

case class BookRecord(userId: String,
                      bookId: String,
                      date: Long,
                      `type`: String, // deposit, withdraw
                      source: String,
                      amount: Float,
                      fee: Float,
                      var currentBalance: Float)

case class RecordSearchCriteria(var bookNames: Option[List[String]] = None,
                                var dateFrom: Option[Long] = None,
                                var dateTo: Option[Long] = None,
                                var userIds: Option[List[String]] = None,
                                var region: Option[String] = None,
                                var sources: Option[List[String]] = None)


case class BookModel(book: Book, records: List[BookRecord]) {
  def getCurrentTotalBalance = if (!records.isEmpty) records(0).currentBalance else 0f
}

case class Book(_id: String = new ObjectId().toString, owner: String, name: String, var balance: Float, createdAt: Long)

object Book {
  def apply(owner: String, name: String): Book = new Book(owner = owner, name = name, balance = 0f,
    createdAt = System.currentTimeMillis())
}

case class BookBrief(id: String, owner: String, name: String, balance: Float)

object BookBrief {
  def apply(book: Book): BookBrief = {
    new BookBrief(book._id, book.owner, book.name, book.balance)
  }
}

class Accounting

object Accounting {
  val logger = Logger[Accounting]
  val booksCollection = CleaMongoClient.getBooksCollection
  val recordsCollection = CleaMongoClient.getBookRecordsCollection

  def createBook(userId: String, bookName: String) = {
    val books = booksCollection.find(and(equal("owner", userId), equal("name", bookName))).first().results()

    if (books != null && books.isEmpty) {
      booksCollection.insertOne(Book(userId, bookName)).results()
    }
  }

  def getBook(bookName: String): Option[BookModel] = {
    val rawBooks = booksCollection.find(and(equal("name", bookName))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty) {
      val book = rawBooks(0)
      val records = recordsCollection.find(and(equal("userId", book.owner), equal("bookId", bookName))).results()

      if (records != null)
        Some(BookModel(book, records.toList))
      else None

    } else None

  }

  def getBookBrief(bookId: String) = {
    val rawBooks = booksCollection.find(and(equal("name", bookId))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty)
      Some(BookBrief(rawBooks(0)))
    else
      None
  }

  def getBalance(bookId: String): Float = {
    val book = getBookBrief(bookId)

    book match {
      case b: Book => b.balance
      case _ => 0f
    }
  }

  def getBooks(userId: String) = {
    val books = new util.ArrayList[BookModel]()
    val rawBooks = booksCollection.find(and(equal("owner", userId))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty) {
      rawBooks foreach (rb => {
        val records = recordsCollection.find(and(equal("bookId", rb.name), equal("userId", userId))).results()
        books.add(BookModel(rb, records.toList))
      })
    }

    books
  }

  def getBookBriefs(userId: String) = {
    val bookBriefs = new util.ArrayList[BookBrief]()
    val rawBooks = booksCollection.find(equal("owner", userId))

    rawBooks foreach { book => bookBriefs.add(BookBrief(book)) }
    bookBriefs
  }

  def deleteBooks(username: String) = {
    booksCollection.deleteMany(equal("owner", username)).results()
    recordsCollection.deleteMany(equal("userId", username)).results()
  }

  def deleteBook(username: String, bookName: String) = {
    booksCollection.deleteMany(and(equal("owner", username), equal("name", bookName))).results()
    recordsCollection.deleteMany(and(equal("userId", username), equal("bookId", bookName))).results()
  }

  def getRecords(recordSearchCriteria: RecordSearchCriteria) = {
    var filters = new ListBuffer[conversions.Bson]()

    recordSearchCriteria.region match {
      case Some(region) => {
        val users = UserManagement.getUsers(UserSearchCriteria(regions = Some(List(region))))

        if (!users.isEmpty)
          filters += in("userId", users map {
            _.username
          })
      }
      case None => ;
    }

    recordSearchCriteria.bookNames match {
      case Some(bookNames) => filters += in("bookId", bookNames: _*)
      case None => ;
    }

    recordSearchCriteria.dateFrom match {
      case Some(date) => filters += gte("date", date)
      case None => ;
    }

    recordSearchCriteria.dateTo match {
      case Some(date) => filters += gte("date", date)
      case None => ;
    }

    recordSearchCriteria.sources match {
      case Some(sources) => filters += in("source", sources: _*)
      case None => ;
    }

    recordSearchCriteria.userIds match {
      case Some(userIds) => filters += in("userId", userIds: _*)
      case None => ;
    }

    if (!filters.isEmpty)
      recordsCollection.find(and(filters: _*)).results()
    else
      recordsCollection.find().results()
  }

  def addRecord(bookId: String, record: BookRecord) = {
    val matchedBooks = booksCollection.find(and(equal("name", bookId), equal("owner", record.userId))).first().results()

    if (matchedBooks.length != 0) {
      val book = matchedBooks(0)

      if (record.`type`.equals("deposit"))
        book.balance = book.balance + record.amount
      else
        book.balance = book.balance - record.amount

      record.currentBalance = book.balance

      booksCollection.replaceOne(and(equal("name", bookId), equal("owner", record.userId)), book,
        new UpdateOptions().upsert(true)).results()

      recordsCollection.insertOne(record).results()

      book
    }
  }

  def requestData = {
    val deals = CleversniperAdapter.getDeals
  }
}
