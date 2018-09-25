package state

import java.io._

import cats.effect.IO

import scala.io.Source

object ExampleIO extends App {

  type Matrix[A] = List[List[A]]

  val listOfInts: IO[Matrix[Int]] = IO (Source
    .fromFile("data.csv")
    .getLines()
    .toList
    .map(_.split(",").toList.map(_.toInt))
  )

  val ints: IO[List[Int]] = IO (Source
    .fromFile("data2.csv")
    .getLines()
    .toList
    .map(_.toInt)
  )

  def toFile(text: String): IO[Unit] = IO {
    val file = new File("out.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
    ()
  }

  def filterBy(allNums: List[List[Int]], validNums: List[Int]) =
    allNums.filter(_.exists(validNums.contains(_))).map(_.mkString(",")).mkString("\n")

  val program =
    for {
      data <- listOfInts
      validNums <- ints
      _ <- toFile(filterBy(data, validNums))
    } yield ()

  program.unsafeRunSync()
}
