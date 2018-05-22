package accounting

import java.time.{LocalDate, LocalDateTime}
import java.util

import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import contracts.Contracts
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import org.mongodb.scala.bson.conversions
import user_management.{UserManagement, UserSearchCriteria}

import scala.collection.mutable.ListBuffer

/**
  * Created by spectrum on 5/14/2018.
  */
case class DepositWithdrawSpec(`type`: String, bookId: String, source: String, amount: Float, fee: Float, note: String)

case class BookRecord(_id: String,
                      userId: String,
                      bookId: String,
                      date: Long,
                      `type`: String, // deposit, withdraw
                      source: String,
                      amount: Float,
                      fee: Float,
                      var currentBalance: Float = 0f)

object BookRecord {
  def apply(userId: String, bookId: String, date: Long, `type`: String, source: String, amount: Float, fee: Float)
  = new BookRecord(new ObjectId().toString, userId, bookId, date, `type`, source, amount, fee, 0f)
}

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

  def distributeProfit(totalProfit: Float, botName: String, date: LocalDate =  LocalDateTime.now().toLocalDate) {
    val contract = Contracts.getContract("talisant", botName)
    val booksOfInterest = Accounting.getBooksByName(botName) filter {_.balance > 0 }
    val botBalance = booksOfInterest map {_.balance} sum

    var talisantProfit = 0f
    if(booksOfInterest.isEmpty)
      talisantProfit = totalProfit
    else {
      talisantProfit = (contract.profitMargin / booksOfInterest.size) * totalProfit
      val talisantProfitRecord = BookRecord("talisant", "profit", date.toEpochDay, "deposit", botName, talisantProfit, 0f)
      Accounting.addRecord("profit", talisantProfitRecord)

      val leftover = totalProfit - talisantProfit

      booksOfInterest foreach {
        book => {
          if (book.balance > 0) {
            val bookProfit = (book.balance / botBalance) * leftover
            val bookProfitRecord = BookRecord(book.owner, "profit", date.toEpochDay, "deposit", botName, bookProfit, 0f)
            Accounting.addRecord("profit", bookProfitRecord)
          }
        }
      }
    }
  }

  def createBook(username: String, name: String) = {
    val books = booksCollection.find(and(equal("owner", username), equal("name", name))).first().results()

    if (books != null && books.isEmpty) {
      booksCollection.insertOne(Book(username, name)).results()
    }
  }

  def getBook(username: String, name: String): Option[BookModel] = {
    val rawBooks = booksCollection.find(and(equal("name", name), equal("owner", username))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty) {
      val book = rawBooks(0)
      val records = recordsCollection.find(and(equal("userId", book.owner), equal("bookId", name))).results()

      if (records != null)
        Some(BookModel(book, records.toList))
      else None

    } else None
  }

  def getBooksByName(name: String) = {
    var books = Seq[Book]()
    val rawBooks = booksCollection.find(equal("name", name)).first().results()

    if (rawBooks != null)
      books = rawBooks

    books
  }

  def getBookBrief(username: String, name: String) = {
    val rawBooks = booksCollection.find(and(equal("owner", username), equal("name", name))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty)
      Some(BookBrief(rawBooks(0)))
    else
      None
  }

  def getBalance(username: String, name: String): Float = {
    val book = getBookBrief(username, name)

    book match {
      case Some(b) => b.balance
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
        book.balance = book.balance + record.amount - record.fee
      else
        book.balance = book.balance - record.amount - record.fee

      record.currentBalance = book.balance

      booksCollection.replaceOne(and(equal("name", bookId), equal("owner", record.userId)), book,
        new UpdateOptions().upsert(true)).results()

      recordsCollection.insertOne(record).results()

      book
    }
  }
}
