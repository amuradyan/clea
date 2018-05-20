package helpers

import accounting.{Accounting, BookRecord}
import adapters.cleversniper.CleversniperAdapter
import contracts.Contracts
import org.quartz.{Job, JobExecutionContext}
import org.quartz.impl.StdSchedulerFactory

/**
  * Created by spectrum on 5/19/2018.
  */
class CleversniperFetcher() extends Job {
  override def execute(context: JobExecutionContext) = {
    val deals = CleversniperAdapter.getDeals

    val totalProfit = deals map {_.Profit} sum

    val contract = Contracts.getContract("talisant", "cleversniper")
    val books = Accounting.getBooksByName("cleversniper")
    val botBalance = books map {_.balance} sum

    val talisantProfit = (contract.profitMargin / books.size) * totalProfit
    val talisantProfitRecord = BookRecord("talisant", "profit", System.currentTimeMillis(), "deposit", "cleversniper", talisantProfit, 0f)
    Accounting.addRecord("profit", talisantProfitRecord)

    val leftover = totalProfit - talisantProfit

    books foreach {
      book => {
        val bookProfit = (book.balance / botBalance) * leftover
        val bookProfitRecord = BookRecord(book.owner, "profit", System.currentTimeMillis(), "deposit", "cleversniper", bookProfit, 0f)
        Accounting.addRecord("profit", bookProfitRecord)
      }
    }
  }
}

object Fetcher {
  val scheduler = StdSchedulerFactory.getDefaultScheduler()

  def setup = {
    import org.quartz.JobBuilder._
    import org.quartz.TriggerBuilder._
    import org.quartz.SimpleScheduleBuilder._

    val job = newJob(classOf[CleversniperFetcher]).withIdentity("cleversniper", "fetchers").build()

    val trigger = newTrigger.withIdentity("cleversniper-trigger", "fetcher-triggers")
      .startNow()
      .withSchedule(simpleSchedule()
        .withIntervalInHours(24)
        .repeatForever())
      .build()

    scheduler.scheduleJob(job, trigger)
//    scheduler.start
  }
}
