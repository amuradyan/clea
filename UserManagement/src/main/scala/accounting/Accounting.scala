package accounting

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util

import com.mongodb.client.model.UpdateOptions
import com.typesafe.scalalogging.Logger
import contracts.Contracts
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import helpers.Validators
import org.mongodb.scala.bson.conversions
import user_management.{UserManagement, UserSearchCriteria}

import scala.collection.mutable.ListBuffer

/**
  * Created by spectrum on 5/14/2018.
  */
case class DepositWithdrawSpec(`type`: String, bookId: String, source: String, amount: Float, fee: Float, note: String) {
  def isValid = {
    `type` != null && `type`.nonEmpty && Validators.isValidType(`type`) &&
     bookId != null && Validators.isValidBookName(bookId) &&
    source != null && Validators.isValidSource(source) &&
    amount > 0 && fee > 0 &&
      ((`type`.equalsIgnoreCase("manual") && Validators.isValidBotName(bookId)) ||
       (!`type`.equalsIgnoreCase("manual") && bookId.equalsIgnoreCase("profit")))
  }

}

case class BookRecord(_id: String,
                      username: String,
                      bookName: String,
                      date: Long,
                      `type`: String,
                      source: String,
                      amount: Float,
                      fee: Float,
                      var currentBalance: Float = 0f)

case class BookRecordExposed(_id: String,
                             userId: String,
                             bookId: String,
                             date: Long,
                             `type`: String,
                             source: String,
                             amount: Float,
                             fee: Float,
                             var currentBalance: Float = 0f,
                             relevantBookBalance: Float = 0f)

object BookRecordExposed {
  def apply(bookRecord: BookRecord, relevantBookBalance: Float): BookRecordExposed =
    new BookRecordExposed(bookRecord._id, bookRecord.username, bookRecord.bookName, bookRecord.date, bookRecord.`type`,
      bookRecord.source, bookRecord.amount, bookRecord.fee, bookRecord.currentBalance, relevantBookBalance)
}

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
  def getCurrentTotalBalance = if (records.nonEmpty) records(0).currentBalance else 0f
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

  def distributeProfit(totalProfit: Float, botName: String, date: LocalDateTime = LocalDateTime.now()) {
    val timestamp = date.atZone(ZoneId.of("GMT+0400")).toInstant.toEpochMilli
    val contract = Contracts.getContract("talisant", botName)

    if (contract != null) {

      val totalProfitWithFee = {
        if (botName == "alpinist")
          totalProfit - totalProfit * 0.02f
        else
          totalProfit
      }

      val booksOfInterest = Accounting.getBooksByName(botName) filter {
        _.balance > 0
      }
      if (booksOfInterest.nonEmpty) {
        val botBalance = booksOfInterest map {
          _.balance
        } sum

        val talisantProfit = contract.profitMargin * totalProfitWithFee
        val talisantProfitRecord = BookRecord("talisant", "profit", timestamp, "deposit", botName, talisantProfit, 0f)
        Accounting.addRecord("profit", talisantProfitRecord)

        val leftover = totalProfitWithFee - talisantProfit

        booksOfInterest foreach {
          book => {
            if (book.balance > 0) {
              val bookProfit = (book.balance / botBalance) * leftover
              val bookProfitRecord = BookRecord(book.owner, "profit", timestamp, "deposit", botName, bookProfit, 0f)
              Accounting.addRecord("profit", bookProfitRecord)
            }
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

    if (rawBooks != null && rawBooks.nonEmpty) {
      val book = rawBooks(0)
      val records = recordsCollection.find(and(equal("userId", book.owner), equal("bookId", name))).results()

      if (records != null)
        Some(BookModel(book, records.toList))
      else None

    } else None
  }

  def getBooksByName(name: String) = {
    var books = Seq[Book]()
    val rawBooks = booksCollection.find(equal("name", name)).results()

    if (rawBooks != null)
      books = rawBooks

    books
  }

  def getBookBrief(username: String, name: String) = {
    val rawBooks = booksCollection.find(and(equal("owner", username), equal("name", name))).first().results()

    if (rawBooks != null && rawBooks.nonEmpty)
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

  def getBooks(username: String) = {
    val books = new util.ArrayList[BookModel]()
    val rawBooks = booksCollection.find(and(equal("owner", username))).first().results()

    if (rawBooks != null && rawBooks.nonEmpty) {
      rawBooks foreach (rb => {
        val records = recordsCollection.find(and(equal("bookId", rb.name), equal("userId", username))).results()
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

        if (users.nonEmpty)
          filters += in("username", users map {
            _.username
          })
      }
      case None => ;
    }

    recordSearchCriteria.bookNames match {
      case Some(bookNames) => filters += in("bookName", bookNames: _*)
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
      case Some(userIds) => filters += in("username", userIds: _*)
      case None => ;
    }

    val recordsOfInterest = {
      if (filters.nonEmpty)
        recordsCollection.find(and(filters: _*)).results()
      else
        recordsCollection.find().results()
    }

    val allBooks = booksCollection.find().results()

    var exposedRecords = ListBuffer[BookRecordExposed]()

    recordsOfInterest foreach {
      record => {
        if (record.source != "manual") {
          logger.info(s"The balance of relevant book filters : owner - ${record.username} and name = ${record.source}")
          val relevantBook = allBooks filter { b => b.owner == record.username && b.name == record.source }
          val balanceAtThatTime = recordsOfInterest filter {
            _.date <= record.date
          } map {
            _.amount
          } sum

          val relevantBookBalance = {
            if (relevantBook.nonEmpty)
              relevantBook.head.balance
            else {
              logger.error(s"Relevant book ${record.bookName} of ${record.username} was not found but record with id ${record._id} exists")
              0f
            }
          }

          exposedRecords += BookRecordExposed(record, relevantBookBalance)
        } else {
          exposedRecords += BookRecordExposed(record, 0)
        }
      }
    }

    exposedRecords sortBy (_.date) toList
  }

  def addRecord(bookName: String, record: BookRecord) = {
    val matchedBooks = booksCollection.find(and(equal("name", bookName), equal("owner", record.username))).first().results()

    if (matchedBooks.length != 0) {
      val book = matchedBooks(0)

      if (record.`type`.equals("deposit"))
        book.balance = book.balance + record.amount - record.fee
      else
        book.balance = book.balance - record.amount - record.fee

      record.currentBalance = book.balance

      booksCollection.replaceOne(and(equal("name", bookName), equal("owner", record.username)), book,
        new UpdateOptions().upsert(true)).results()

      recordsCollection.insertOne(record).results()

      book
    }
  }
}
