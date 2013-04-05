import emissions._
import java.io.File

object test {


  val countsIn = new File("/Users/dragos/Documents/workspace-nlp/hmm/gene.counts.rare")
                                                  //> countsIn  : java.io.File = /Users/dragos/Documents/workspace-nlp/hmm/gene.co
                                                  //| unts.rare
  val inputFile = new File("/Users/dragos/Documents/workspace-nlp/hmm/gene.train.rare")
                                                  //> inputFile  : java.io.File = /Users/dragos/Documents/workspace-nlp/hmm/gene.t
                                                  //| rain.rare

  val tagger = new SimpleTagger(countsIn, inputFile)
                                                  //> tagger  : emissions.SimpleTagger = emissions.SimpleTagger@2ce1899b
  
  tagger.q("STOP", "*", "*")                      //> res0: Double = NaN
}