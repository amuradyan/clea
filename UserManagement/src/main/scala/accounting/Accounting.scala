package accounting

import java.util

import com.typesafe.scalalogging.Logger
import org.bson.types.ObjectId

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

case class Book(_id: String = new ObjectId().toString, owner: String, name: String, records: util.ArrayList[BookRecord] = new util.ArrayList[BookRecord]())

object Book {
  def apply(owner: String, name: String): Book = new Book(owner = owner, name = name)
}

case class BookBrief(id: String, owner: String, name: String, balance: Float)

class Accounting
object Accounting {
  val logger = Logger[Accounting]

  def createBook(userId: String, name: String)  = ???
  def getBookBrief(bookId: String): BookBrief = ???
  def getBalance(bookId: String): Float = ???
  def getBookBriefs(userId: String) = ???
  def getRecords(recordSearchCriteria: RecordSearchCriteria) = ???
  def addRecord(bookId: String, record: BookRecord) = ???
}
