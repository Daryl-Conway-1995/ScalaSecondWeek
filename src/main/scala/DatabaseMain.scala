import slick.jdbc.MySQLProfile.api._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}


object DatabaseMain extends App {

  // The config string refers to mysqlDB that we defined in application.conf
  val db = Database.forConfig("mysqlDB")

  // represents the actual table on which we will be building queries on
  val peopleTable = TableQuery[People]

  // schema definition to generate DROP statement for people table
  val dropPeopleCmd = DBIO.seq(peopleTable.schema.drop)

  // schema definition to generate a CREATE TABLE command
  val initPeopleCmd = DBIO.seq(peopleTable.schema.create)

  def dropDB = {
    //do a drop followed by initialisePeople
    val dropFuture = Future {
      db.run(dropPeopleCmd)
    }
    //Attempt to drop the table, Await does not block here
    Await.result(dropFuture, Duration.Inf).andThen {
      case Success(_) => initialisePeople
      case Failure(error) => println("Dropping the table failed due to: " + error.getMessage)
        initialisePeople
    }
  }

  def initialisePeople = {
    //initialise people
    val setupFuture = Future {
      db.run(initPeopleCmd)
    }
    // once our DB has finished initializing we are ready to roll, Await does not block
    Await.result(setupFuture, Duration.Inf).andThen {
      case Success(_) => runQuery
      case Failure(error) => println("Initialising the table failed due to: " + error.getMessage)
    }
  }

  def runQuery = {
    val insertPeople = Future {
      val query = peopleTable ++= Seq(
        (10, "Jack", "Wood", 36),
        (20, "Tim", "Brown", 24),
        (30, "Tim", "Wood", 24),
        (40, "Jack", "Wooden", 36),
        (50, "Jack", "Woodrow", 36),

      )
      //insert into `PEOPLE` (`PER_FNAME`,`PER_LNAME`,`PER_AGE`)  values (?,?,?)
      println(query.statements.head) // would print out the query one line up
      db.run(query)
    }
    Await.result(insertPeople, Duration.Inf).andThen {
      case Success(_) => mostCommonName()
        listPeople
      case Failure(error) => println("Welp! Something went wrong! " + error.getMessage)
    }
  }

  def listPeople = {
    val queryFuture = Future {
      //simple query that selects everything from People and prints them out
      db.run(peopleTable.result).map(_.foreach {
        case (id, fName, lName, age) => println(s" $id $fName $lName $age")
      })
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => db.close() //cleanup DB connection
      case Failure(error) => println("Listing people failed due to: " + error.getMessage)
    }
  }

  def peopleOlderThen(searchAge: Int) = {
    val queryFuture = Future {
      //simple query that selects People older then a given age prints them out
      db.run(peopleTable.result).map(_.foreach {
        case (id, fName, lName, age) if age > searchAge => println(s" $id $fName $lName $age")
        case _ =>
      })
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println("people older then _ was successful")
      case Failure(error) => println("people older then _ failed due to: " + error.getMessage)
    }

  }

  def peopleNamed(firstName: String) = {
    val queryFuture = Future {
      db.run(peopleTable.result).map(_.foreach {
        case (id, fName, lName, age) if fName equals firstName => println(s" $id $fName $lName $age")
        case _ =>
      })
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println("people named _ was successful")
      case Failure(error) => println("people named _ failed due to: " + error.getMessage)
    }
  }

  def peopleLastName(lastName: String) = {
    val queryFuture = Future {
      db.run(peopleTable.result).map(_.foreach {
        case (id, fName, lName, age) if lName equals lastName => println(s" $id $fName $lName $age")
        case _ =>
      })
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println("people last name _ was successful")
      case Failure(error) => println("people last name _ failed due to: " + error.getMessage)
    }
  }

  def editAge(index: Int, newAge: Int) = {
    val query1 = for (
      c <- peopleTable if c.id === index)
      yield c.age
    val queryFuture = Future {
      db.run(query1.update(newAge))
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println("edit age was successful")
      case Failure(error) => println("edit age failed due to: " + error.getMessage)
    }
  }

  def deletePerson(index: Int) = {
    val query1 = peopleTable.filter(_.id === index)
    val queryFuture = Future {
      db.run(query1.delete)
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println(s"person at index $index has been deleted")
      case Failure(error) => println("delete person failed due to: " + error.getMessage)
    }
  }

  def countPeople() = {
    var numberOfPeople = 0
    val getRows = peopleTable.length.result
    Await.result(db.run(getRows).map(v => numberOfPeople = v), Duration.Inf)
    numberOfPeople
  }

  def averageAge(): Unit = {
    val query1 = db.run(peopleTable.map(_.age).avg.result)
    Await.result(query1.map(println), Duration.Inf)
  }

  def mostCommonName() = {
    val queryFuture = Future {
      db.run(peopleTable.groupBy(_.fName).map {
        case (fName, occurrence) => fName -> occurrence.length
      }.result).map({case a => println(a.maxBy(a=>a._2))})
    }
    Await.result(queryFuture, Duration.Inf).andThen {
      case Success(_) => println()
      case Failure(exception) => println(exception)
    }
  }

  dropDB
  Thread.sleep(10000)

}
