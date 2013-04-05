package emissions

trait Tag

case object GENE extends Tag {
  override def toString() = "I-GENE"
}
case object O extends Tag {
  override def toString() = "O"
}

case class Tagged(word: String, tag: Tag, count: Int)