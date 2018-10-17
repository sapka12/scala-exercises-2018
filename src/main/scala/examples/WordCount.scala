package examples

import scala.io.Source

object WordCount extends App {

  val wordsPath = "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt"

  def anagram(path: String): List[String] = Source
    .fromURL(path)
    .getLines
    .toList
    .groupBy(_.sorted)
    .toList
    .sortBy(_._2.length)
    .reverse
    .take(5)
    .map(_._2.mkString(", "))

//  anagram(wordsPath).foreach(println)

  def reverse_words(path: String): List[String] = {
    Source
      .fromURL(path)
      .getLines.toList.map(_.toLowerCase).filter(x => x.reverse == x).sortBy(-1 * _.length).take(5)
  }

//  reverse_words(wordsPath).foreach(println)


//  def isChapter(line: String): Boolean = line.startsWith("Chapter")
//
//  val prideAndPrejudice = "http://www.gutenberg.org/files/1342/1342-0.txt"
//
//  type Person = String
//  type Connection = (Person, Person)
//  type Connections = List[Connection]
//
//  def isPerson(name: String): Boolean = name.headOption.filter(_.isUpper).isDefined
//
//  def conn(head: Person, tail: List[Person]): Connections =
//    if (isPerson(head)) List()
//    else tail.filter(isPerson).map((head, _))
//
//  def connections(path: String): List[Connections] = {
//
//    val areaSize = 20
//
//    val prideSource = Source.fromURL(path)
//
//    val removable = ".”“!?;:,".toSet
//
//    val chapters = prideSource.getLines
//      .filter(line => !line.trim.isEmpty)
//      .foldLeft[List[String]](List.empty[String]){
//      case (aggList, line) =>
//        if (isChapter(line)) aggList ::: List("")
//        else if (aggList.isEmpty) aggList
//        else aggList.init ::: List(aggList.last + " " + line)
//    }.map(_.filterNot(removable))
//
//    def connectionsInChapter(chapter: String): Connections = {
//      val words = chapter.split(" ").toList
//      words.zipWithIndex.flatMap{
//        case (_, idx) =>
//          val area = words.drop(idx).take(areaSize)
//          conn(area.head, area.tail)
//      }
//    }
//
//    chapters.map(connectionsInChapter)
//  }
//
//  connections(prideAndPrejudice).foreach(println)

}
