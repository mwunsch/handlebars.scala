package com.gilt.handlebars

import com.gilt.handlebars._

import java.io.File
import java.util.Random

import org.specs2.mutable._

import scala.io._

class HandlebarsPerfSpec extends Specification {

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

  "handlebars" should {
    "render a template in less than " + Threshold + " ms" in {
      val count = 25000
      var totalSize = 0l
      var totalRuntime = 0l

      val handlebars = Handlebars(Template)

      // warmup the jvm (let the JIT find the hotspots)
      for (i <- 0 to count / 100) {
        val context = generateRndFriends(rndGen)
        val size = handlebars(context).size
        totalSize += size // you must use what you build or the JVM will optimize it away
      }

      for (i <- 0 to count) {
        val context = generateRndFriends(rndGen)
        val now = System.currentTimeMillis
        val actual = handlebars(context)
        val elapsed = (System.currentTimeMillis - now)
        totalRuntime += elapsed
        totalSize += actual.size
      }

      val avg = totalRuntime / count
      println("Average rendering time is: " + avg + " ms.")

      avg.toInt must beLessThan(5)
    }
  }
}
