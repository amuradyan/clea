package accounting

import java.util

import com.typesafe.scalalogging.Logger
import mongo.CleaMongoClient
import org.bson.types.ObjectId
import org.mongodb.scala.model.Filters._
import helpers.Helpers._
import scala.collection.JavaConversions._

/**
  * Created by spectrum on 5/14/2018.
  */
case class DepositWithdrawSpec(`type`: String, bookId: String, source: String, amount: Float, fee: Float, note: String)

case class BookRecord(userId: String,
                      bookId: String,
                      date: Long,
                      recordType: String, // deposit, withdraw
                      source: String,
                      amount: Float,
                      fee: Float,
                      currentTotalBalance: Float)

case class RecordSearchCriteria(var bookNames: Option[util.ArrayList[String]] = None,
                                var dateFrom: Option[Long] = None,
                                var dateTo: Option[Long] = None,
                                var userIds: Option[util.ArrayList[String]] = None,
                                var region: Option[String] = None,
                                var sources: Option[util.ArrayList[String]] = None)

case class Book(_id: String = new ObjectId().toString, owner: String, name: String, records: Seq[BookRecord]) {
  def getCurrentTotalBalance = if(!records.isEmpty) records(0).currentTotalBalance else 0f
}

object Book {
  def apply(owner: String, name: String): Book = new Book(owner = owner, name = name, records = Seq[BookRecord]())
}

case class BookBrief(id: String, owner: String, name: String, balance: Float)

object BookBrief {
  def apply(book: Book): BookBrief = {
    new BookBrief(book._id, book.owner, book.name, book.getCurrentTotalBalance)
  }
}

class Accounting
object Accounting {
  val logger = Logger[Accounting]
  val booksCollection = CleaMongoClient.getBooksCollection

  def createBook(userId: String, bookName: String)  = {
    val books = booksCollection.find(and(equal("owner", userId), equal("name", bookName))).first().results()

    if (books != null && books.isEmpty) {
      booksCollection.insertOne(Book(userId, bookName)).results()
    }
  }
  def getBookBrief(bookId: String): Option[BookBrief] = {
    val rawBooks = booksCollection.find(and(equal("_id", bookId))).first().results()

    if (rawBooks != null && rawBooks.isEmpty)
      Some(BookBrief(rawBooks(0)))
    else
      None

  }

  def getBalance(bookId: String): Float = ???

  def getBookBriefs(userId: String) = {
    val bookBriefs = new util.ArrayList[BookBrief]()
    val rawBooks = booksCollection.find(and(equal("owner", userId))).first().results()

    if (rawBooks != null && !rawBooks.isEmpty)
      rawBooks foreach( rb => bookBriefs.add(BookBrief(rb)))

    bookBriefs
  }

  def getRecords(recordSearchCriteria: RecordSearchCriteria) = ???
  def addRecord(bookId: String, record: BookRecord) = ???
}
