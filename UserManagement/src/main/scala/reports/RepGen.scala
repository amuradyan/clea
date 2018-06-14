package reports

import java.time.{Instant, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

import com.itextpdf.text.{Document, FontFactory, PageSize, Phrase}
import com.itextpdf.text.pdf.PdfPCell
import com.itextpdf.text.pdf.PdfPTable
import com.itextpdf.text.pdf.PdfWriter
import java.io.FileOutputStream
import java.text.DecimalFormat

import accounting.BookRecordExposed
import com.norbitltd.spoiwo.model._
import com.norbitltd.spoiwo.model.enums.CellBorderStyle
import com.norbitltd.spoiwo.natures.xlsx.Model2XlsxConversions._
import com.typesafe.scalalogging.Logger
import user_management.{User, UserExposed, UserManagement}

import scala.collection.mutable.ListBuffer

/**
  * Created by spectrum on 6/5/2018.
  */

class UnsupportedFormat extends Throwable

object CellFactory {
  def thickCell(value: String) = Cell(value, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thick, topStyle = CellBorderStyle.Thick, rightStyle = CellBorderStyle.Thick, bottomStyle = CellBorderStyle.Thick)))

  def thinCell(value: String) = Cell(value, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thin, topStyle = CellBorderStyle.Thin, rightStyle = CellBorderStyle.Thin, bottomStyle = CellBorderStyle.Thin)))

  def thickCell(index: Int) = Cell("", index = index, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thick, topStyle = CellBorderStyle.Thick, rightStyle = CellBorderStyle.Thick, bottomStyle = CellBorderStyle.Thick)))

  def thinCell(index: Int) = Cell("", index = index, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thin, topStyle = CellBorderStyle.Thin, rightStyle = CellBorderStyle.Thin, bottomStyle = CellBorderStyle.Thin)))

  def thickCell(value: String, index: Int) = Cell(value, index = index, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thick, topStyle = CellBorderStyle.Thick, rightStyle = CellBorderStyle.Thick, bottomStyle = CellBorderStyle.Thick)))

  def thinCell(value: String, index: Int) = Cell(value, index = index, style = CellStyle(borders = CellBorders(leftStyle = CellBorderStyle.Thin, topStyle = CellBorderStyle.Thin, rightStyle = CellBorderStyle.Thin, bottomStyle = CellBorderStyle.Thin)))
}

class RepGen

object RepGen {

  private val logger = Logger[RepGen]
  private val dtf = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  object Formats extends Enumeration {
    val PDF = "pdf"
    val XLSX = "xlsx"
  }

  /**
    * Report header and footer
    *
    * +-------+--------------+------+---------------------+------------+--------------+---------------------+--------+
    * |   N   | Name/Surname | Date |        Deposit      |  Alpinist  | CleverSniper |      Withdrawal     | Profit |
    * +-------+--------------+------+--------+-----+------+-----+------+------+-------+--------+-----+------+--------+
    * |       |              |      | Amount | Fee | Fee% | P/L | P/L% |  P/L | P/L%  | Amount | Fee | Fee% |        |
    * +-------+--------------+------+--------+-----+------+-----+------+------+-------+--------+-----+------+--------+
    * |                                                 ...........                                                  |
    * |                                                 ...........                                                  |
    * +-------+--------------+------+--------+-----+------+-----+------+------+-------+--------+-----+------+--------+
    * | Total |              |      |        |     |      |     |      |      |       |        |     |      |        |
    * +-------+--------------+------+--------+-----+------+-----+------+------+-------+--------+-----+------+--------+
    *
    **/
  private val reportHeader = List(
    Row(),
    Row(
      CellFactory.thickCell("N", 0),
      CellFactory.thickCell("Name/Surename", 1),
      CellFactory.thickCell("Date", 2),
      CellFactory.thickCell("Deposit", 3),
      CellFactory.thickCell(4),
      CellFactory.thickCell(5),
      CellFactory.thickCell("Alpinist", index = 6),
      CellFactory.thickCell(7),
      CellFactory.thickCell("CleverSniper", 8),
      CellFactory.thickCell(9),
      CellFactory.thickCell("Withdrawal", 10),
      CellFactory.thickCell(11),
      CellFactory.thickCell(12),
      CellFactory.thickCell("Profit", 13)
    ),

    Row(
      CellFactory.thickCell(""),
      CellFactory.thickCell(""),
      CellFactory.thickCell(""),
      CellFactory.thickCell("Amount"),
      CellFactory.thickCell("Fee"),
      CellFactory.thickCell("Fee%"),
      CellFactory.thickCell("P/L"),
      CellFactory.thickCell("P/L%"),
      CellFactory.thickCell("P/L"),
      CellFactory.thickCell("P/L%"),
      CellFactory.thickCell("Amount"),
      CellFactory.thickCell("Fee"),
      CellFactory.thickCell("Fee%"),
      CellFactory.thickCell("")
    )
  )

  private def reportFooter(total: Float) = Row(
    CellFactory.thickCell("Total"),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(""),
    CellFactory.thickCell(total.toString)
  )

  private def generateRow(index: Int, user: User, record: BookRecordExposed) = {
    val indexCell = CellFactory.thickCell(index.toString, 0)
    val nameSurnameCell = CellFactory.thinCell(s"${user.name} ${user.surname}", 1)
    val dateCell = CellFactory.thinCell(LocalDateTime.ofInstant(Instant.ofEpochMilli(record.date), ZoneId.of("GMT+0400")).format(dtf), 2)
    var depositAmountCell = CellFactory.thinCell(3)
    var depositFeeCell = CellFactory.thinCell(4)
    var depositFeePercentageCell = CellFactory.thinCell(5)
    var alpinistPLCell = CellFactory.thinCell(6)
    var alpinistPLPercentageCell = CellFactory.thinCell(7)
    var cleversniperPLCell = CellFactory.thinCell(8)
    var cleversniperPLPercentageCell = CellFactory.thinCell(9)
    var withdrawAmountCell = CellFactory.thinCell(10)
    var withdrawFeeCell = CellFactory.thinCell(11)
    var withdrawFeePercentageCell = CellFactory.thinCell(12)

    val formatter = new DecimalFormat("#######.##")
    val percentageFormatter = new DecimalFormat("###.######")

    record.source match {
      case "manual" => {
        record.`type` match {
          case "deposit" => {
            depositAmountCell = CellFactory.thinCell(formatter.format(record.amount), 3)
            depositFeeCell = CellFactory.thinCell(formatter.format(record.fee), 4)
            depositFeePercentageCell = CellFactory.thinCell(percentageFormatter.format((record.fee / record.amount)), 5)
          }
          case "withdraw" => {
            withdrawAmountCell = CellFactory.thinCell(formatter.format(record.amount), 10)
            withdrawFeeCell = CellFactory.thinCell(formatter.format(record.fee), 11)
            withdrawFeePercentageCell = CellFactory.thinCell(percentageFormatter.format((record.fee / record.amount)), 12)
          }
        }
      }
      case "alpinist" => {
        alpinistPLCell = CellFactory.thinCell(formatter.format(record.amount), 6)

        if (user.username != "talisant")
          alpinistPLPercentageCell = CellFactory.thinCell(percentageFormatter.format((record.amount / record.relevantBookBalance)), 7)
      }
      case "cleversniper" => {
        cleversniperPLCell = CellFactory.thinCell(formatter.format(record.amount), 8)

        if (user.username != "talisant")
          cleversniperPLPercentageCell = CellFactory.thinCell(percentageFormatter.format((record.amount / record.relevantBookBalance)), 9)
      }
    }

    val profitCell = CellFactory.thickCell(formatter.format(record.currentBalance), 13)


    Row(
      indexCell,
      nameSurnameCell,
      dateCell,
      depositAmountCell,
      depositFeeCell,
      depositFeePercentageCell,
      alpinistPLCell,
      alpinistPLPercentageCell,
      cleversniperPLCell,
      cleversniperPLPercentageCell,
      withdrawAmountCell,
      withdrawFeeCell,
      withdrawFeePercentageCell,
      profitCell
    )
  }

  private def saveToFS(report: Sheet, format: String) = {
    val name = s"report-${System.currentTimeMillis()}.$format"

    format match {
      case Formats.PDF => {
        val xlsxReport = report.convertAsXlsx()

        val worksheet = xlsxReport.getSheetAt(0)
        val pdfReport = new Document(PageSize.A4.rotate())
        val reportTable = new PdfPTable(14)
        PdfWriter.getInstance(pdfReport, new FileOutputStream(name))
        pdfReport.open
        reportTable.setTotalWidth(Array[Float](0.6f, 1.4f, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

        worksheet.iterator forEachRemaining {
          row => {
            row.iterator forEachRemaining {
              cell => {
                val pdfCell = new PdfPCell(new Phrase(cell.getStringCellValue, FontFactory.getFont(FontFactory.COURIER, 6)))
                reportTable.addCell(pdfCell)
              }
            }
          }
        }

        pdfReport.add(reportTable)
        pdfReport.close
      }
      case Formats.XLSX => report.saveAsXlsx(name)
      case _ => throw new UnsupportedFormat
    }
  }

  private def generateReport(rows: List[Row], format: String) = {
    Sheet().withRows(rows)
      .withColumns(Column(1, autoSized = true))
      .withMergedRegions(CellRange(1 -> 1, 3 -> 5), CellRange(1 -> 1, 6 -> 7), CellRange(1 -> 1, 8 -> 9), CellRange(1 -> 1, 10 -> 12))
  }


  def generateRows(records: List[BookRecordExposed]) = {
    val rows = ListBuffer[Row]()

    rows.++=(reportHeader)

    val total = records map {
      _.amount
    } sum

    var i = 0
    val users = UserManagement.getAllUsers

    logger.info(s"${records.length} records fetched ***************************")

    records foreach {
      r => {
        i = i + 1
        val u = users filter {
          _.username == r.userId
        }

        if (u.nonEmpty)
          rows += generateRow(i, u(0), r)
        else
          logger.error(s"User ${r.userId} not found")
      }
    }

    rows += reportFooter(total)

    rows.toList
  }

  def generate(records: List[BookRecordExposed], format: String) = {
    val rows = generateRows(records)
    val report = generateReport(rows, format)
    saveToFS(report, format)
  }
}
