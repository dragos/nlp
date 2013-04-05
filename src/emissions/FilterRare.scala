package emissions

import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap

trait TrainingData
case class TaggedWord(word: String, tag: String) extends TrainingData
case object EmptyLine extends TrainingData

class FilterRare(in: File) {

  private def inputData(in: File): Seq[TrainingData] = {
    val extract = """(.*) (.*)""".r
    val src = Source.fromFile(in)
    try
      for {
        line <- Source.fromFile(in).getLines.toList
      } yield {
        if (line == "")
          EmptyLine
        else {
          val extract(word, tag) = line
          TaggedWord(word, tag)
        }
      }
    finally src.close
  }

  val data = inputData(in)
  val counts = HashMap[String, Int]()

  data collect {
    case TaggedWord(word, _) =>
      counts(word) = counts.getOrElse(word, 0) + 1
  }
  
  def isRareWord(word: String): Boolean =
    counts.get(word).getOrElse(0) < 5

  def filterRare {
    data foreach {
      case TaggedWord(word, tag) =>
        if (isRareWord(word)) println(s"_RARE_ $tag")
        else println(s"$word $tag")
      case _ => println()
    }
  }
}