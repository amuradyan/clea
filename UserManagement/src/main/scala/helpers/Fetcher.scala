package helpers

import adapters.alpinist.adapter.AlpinistAdapterV2
import adapters.cleversniper.adapter.CleversniperAdapter
import org.quartz.impl.StdSchedulerFactory
import org.quartz.{Job, JobExecutionContext}

/**
  * Created by spectrum on 5/19/2018.
  */
class AlpinistFetcher extends Job {
  override def execute(context: JobExecutionContext) = {
    AlpinistAdapterV2.run
  }
}

class CleversniperFetcher() extends Job {
  override def execute(context: JobExecutionContext) = {
    CleversniperAdapter.run
  }
}

object Fetcher {
  val scheduler = StdSchedulerFactory.getDefaultScheduler()

  private def setupCleversniperFetcher {
    import org.quartz.JobBuilder._
    import org.quartz.SimpleScheduleBuilder._
    import org.quartz.TriggerBuilder._

    val job = newJob(classOf[CleversniperFetcher]).withIdentity("cleversniper", "fetchers").build()

    val trigger = newTrigger.withIdentity("cleversniper-trigger", "fetcher-triggers")
      .startNow()
      .withSchedule(simpleSchedule()
        .withIntervalInHours(12)
        .repeatForever())
      .build()
    scheduler.scheduleJob(job, trigger)
  }

  private def setupAlpinistFetcher {
    import org.quartz.JobBuilder._
    import org.quartz.SimpleScheduleBuilder._
    import org.quartz.TriggerBuilder._

    val job = newJob(classOf[AlpinistFetcher]).withIdentity("alpinist", "fetchers").build()

    val trigger = newTrigger.withIdentity("alpinist-trigger", "fetcher-triggers")
      .startNow()
      .withSchedule(simpleSchedule()
        .withIntervalInHours(12)
        .repeatForever())
      .build()
    scheduler.scheduleJob(job, trigger)
  }

  def setup = {
    setupAlpinistFetcher
    setupCleversniperFetcher

    scheduler.start()
  }
}


