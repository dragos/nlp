package emissions

import java.io.File
import scala.io.Source

object MainFilter extends App {

   (new FilterRare(new File(args(0)))).filterRare
}

object MainTagger extends App {

  val tagger = new SimpleTagger(new File(args(0)), new File(args(1)))

  for {
    line <- Source.fromFile(new File(args(2))).getLines.toList
  } {
    if (line == "")
      println
    else {
      println(s"$line ${tagger.tag(line)}")
    }
  }
}