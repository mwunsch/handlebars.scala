package com.gilt.handlebars.scala

import java.util.{Date, Random}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{LinkedBlockingQueue, ArrayBlockingQueue, CountDownLatch, Executors}

import org.scalatest.{FunSpec, Matchers}

/**
 * User: chicks
 * Date: 8/23/14
 */
class HandlebarsPerfSpec extends FunSpec with Matchers {
  private object Constants {
    val Threshold = 5 // fail if avg time is gt 5ms
    val Template =
      """
      <div class="friends">
        <h1>These are all my friends</h1>
          {{#each friends}}
          <div class="person">
            <h1>Hello my name is {{name}} {{surname}} and I'm {{age}} years old</h1>
            <h2>I'm currently work for
              {{#job}}
              <h3>Title: {{{title}}}</h3>
              <h3>Department: {{{department}}}</h3>
              <h3>Years: {{{years}}}</h3>
              <h3>Salary: {{{salary}}}</h3>
              {{/job}}
            <h2>My little kids are:</h2>
          <ul>
          {{#kids}}
          <li>Name: {{name}}, age: {{age}}</li>
          {{/kids}}
        </ul>
      </h2>
    </div>
    {{/each}}
    </div>
      """
  }

  // model used for the above template
  case class Friends(friends: Seq[Person])
  case class Job(title: String, department: String, years: Integer, salary: Long)
  case class Person(name: String, surname: String, age: Integer, job: Option[Job], kids: Seq[Map[String, String]])

  private val rndGen = new java.util.Random(123) // use the same seed to reproduce the test
  private def rndString(rnd: Random) = new java.math.BigInteger(32, rnd).toString

  def generateRndSeqOfKids(rnd: Random): Seq[Map[String, String]] =
    (for { i <- 0 to rnd.nextInt(3) } yield {
      Map("name" -> rndString(rnd), "age" -> rnd.nextInt(100).toString)
    }).toList

  def generateRndJob(rnd: Random): Option[Job] =
    rnd.nextBoolean match {
      case true => Some(Job(
        title = rndString(rnd),
        department = rndString(rnd),
        years = rnd.nextInt(100),
        salary = rnd.nextLong()
      ))
      case false => None
    }

  def generateRndPerson(rnd: Random): Person =
    Person(
      name = rndString(rnd),
      surname = rndString(rnd),
      age = rnd.nextInt(),
      job = generateRndJob(rnd),
      kids = generateRndSeqOfKids(rnd)
    )

  def generateRndFriends(rnd: Random): Friends =
    Friends(
      for { i <- 0 to rnd.nextInt(100) } yield {
        generateRndPerson(rnd)
      }
    )

  import Constants._
  import com.gilt.handlebars.scala.binding.dynamic._

  describe("handlebars") {
    it(s"render a template in less than $Threshold ms") {
      val handlebars = Handlebars(Template)

      // warmup the jvm (let the JIT find the hotspots)
      for (i <- 0 to 1000) {
        val context = generateRndFriends(rndGen)
        handlebars(context).size
      }

      val poison = Friends(Nil)
      val source = new ArrayBlockingQueue[Friends](1024)
      val threads = Runtime.getRuntime.availableProcessors()
      val latch = new CountDownLatch(threads)
      val executor = Executors.newFixedThreadPool(threads + 1)
      val count = 50000

      executor.submit(new Runnable {
        override def run() {
          for (i <- 1 to count) {
            source.put(generateRndFriends(rndGen))
          }
          source.put(poison)
        }
      })

      case class Stats(n: Long, sum: Long, sumSq: Double)
      object PoisonStats extends Stats(0,0,0)
      val sink = new LinkedBlockingQueue[Stats]
      val remaining = new AtomicInteger(threads)
      class Worker extends Runnable {
        override def run() {
          var done = false
          var n = 0l
          var sum = 0l
          var sumSq = 0d
          while (!done) {
            val friend = source.take()
            if (friend eq poison) {
              val stats = Stats(n, sum, sumSq)
              sink.put(stats)
              if (remaining.decrementAndGet() == 0) {
                sink.put(PoisonStats)
              } else {
                source.put(friend)
              }
              latch.countDown()
              done = true
            } else {
              val start = System.currentTimeMillis()
              handlebars(friend)
              val elapsed = System.currentTimeMillis() - start
              n += 1
              sum += elapsed
              sumSq += elapsed * elapsed
            }
          }
        }
      }

      val start = System.currentTimeMillis()
      for (i <- 1 to threads) {
        executor.submit(new Worker)
      }
      latch.await()
      val elapsed = System.currentTimeMillis() - start
      var n = 0l
      var sum = 0l
      var sumSq = 0d
      sink.toArray(new Array[Stats](threads)).filter(_ ne PoisonStats).foreach {
        stat =>
          n += stat.n
          sum += stat.sum
          sumSq += stat.sumSq
      }

      val avg = sum / n.toDouble
      val sd = math.sqrt((n * sumSq - sum * sum) / (n * (n - 1d)))
      println("-------------------------------------------")
      println(s"Date: ${new Date().toString}")
      println("Average: %f ms +/- %f ms".format(avg, sd))
      println("Total elapsed = %d ms (avg = %f ms)".format(elapsed, elapsed / count.toDouble))

      avg.toInt should be < 999
    }
  }
}
