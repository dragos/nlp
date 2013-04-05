package emissions

import java.io.File
import scala.io.Source
import scala.collection.mutable

class SimpleTagger(countsFile: File, trainingFile: File) {

  private val extract = """(\d+) WORDTAG (.+) (.+)""".r

  def load(): Seq[Tagged] = {
    val source = Source.fromFile(countsFile)
    try
      for {
        line <- source.getLines().toList
        if (line.contains("WORDTAG"))
        extract(count, tag, word) = line
      } yield tag match {
        case "I-GENE" => Tagged(word, GENE, count.toInt)
        case "O" => Tagged(word, O, count.toInt)
      }
    finally
      source.close()
  }

  private var bigrams: mutable.Map[(String, String), Int] = new mutable.HashMap
  private var trigrams: mutable.Map[(String, String, String), Int] = new mutable.HashMap
  loadMultiGrams()

  private def loadMultiGrams() {
    val source = Source.fromFile(countsFile)
    try
      for {
        line <- source.getLines().toList
        if (!line.contains("WORDTAG"))
      } if (line contains "2-GRAM")
        addBigram(line)
      else if (line contains "3-GRAM") addTrigram(line)
    finally
      source.close()
  }

  private def addBigram(line: String) {
    //    13 2-GRAM I-GENE STOP
    println(line)
    val bigramRex = """(\d+) 2-GRAM (.*) (.*)""".r
    val bigramRex(count, y1, y2) = line
    println(count, y1, y2)
    bigrams((y1, y2)) = count.toInt
  }

  private def addTrigram(line: String) {
    //    12451 3-GRAM * O O
    val bigramRex = """(\d+) 3-GRAM (.*) (.*) (.*)""".r
    val bigramRex(count, y1, y2, y3) = line
    trigrams((y1, y2, y3)) = count.toInt
  }

  def biGram(y1: String, y2: String): Int =
    bigrams.get((y1, y2)).getOrElse(0)

  def triGram(y1: String, y2: String, y3: String): Int =
    trigrams.get((y1, y2, y3)).getOrElse(0)

  //  def emission(): Map[(String, Tag), Double] = {
  //    def sumCounts(ts: Seq[Tagged]) = {
  //      var sum = 0
  //      var xs = ts
  //      while (xs.nonEmpty) {
  //        sum += xs.head.count
  //        xs = xs.tail
  //      }
  //      sum
  //    }
  //    val tagged = load()
  //
  //    // tag -> count
  //    val counts = tagged.groupBy(_.tag).map {
  //      case (k, v) => k -> sumCounts(v)
  //    }
  //    (for {
  //      Tagged(word, tag, count) <- tagged
  //    } yield (word, tag) -> count.toDouble / counts(tag)).toMap
  //  }

  private val tagged = load()
  private val taggedByWord = tagged.groupBy(_.word)
  private var countTags = Map[Tag, Int]()

  private val inputData = new FilterRare(trainingFile)

  tagged collect {
    case Tagged(word, tag, count) => countTags = countTags.updated(tag, countTags.getOrElse(tag, 0) + count)
  }

  def emi(w: String, tag: Tag): Double = {
    val word =
      if (inputData.isRareWord(w)) "_RARE_"
      else w

    taggedByWord.get(word) match {
      case Some(tags) =>
        // count(w | tag)
        val c1 = tags.find(_.tag == tag).map(_.count).getOrElse(0)
        c1.toDouble / countTags(tag)
      case None =>
        error("Shouldn't happen!")
    }
  }

  def tag(word: String): Tag = {
    val e_gene = emi(word, GENE)
    val e_O = emi(word, O)

    if (e_gene > e_O) GENE else O
  }
  
  /**
   * Return the maximum likelihood of tag `z` following tags x, y.
   */
  def q(z: String, x: String, y: String): Double = {
    println(s"${triGram(x, y, z)} / ${biGram(x, y)}")
    triGram(x, y, z).toDouble / biGram(x, y)
  }
}