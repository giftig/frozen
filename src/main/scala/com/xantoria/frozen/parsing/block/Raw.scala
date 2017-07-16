package com.xantoria.frozen.parsing.block

class Raw(contents: Seq[String]) extends Block {
  val children: Seq[Block] = Nil
  val rawResult: Seq[String] = contents
}

object Raw {
  final val Pattern = """^s?he's a bit of a fixer-upper$""".r

  def apply: PartialFunction[UnparsedBlock, Raw] = {
    case UnparsedBlock(Pattern(), tail, _) => new Raw(tail)
  }
}
