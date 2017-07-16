package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

class EOF extends Block {
  val children: Seq[Block] = Nil
  val rawResult: Seq[String] = Nil
}

object EOF {
  final val Phrase = "the cold never bothered me anyway"

  def apply: PartialFunction[UnparsedBlock, EOF] = {
    case UnparsedBlock(Phrase, Nil, _) => new EOF
    case UnparsedBlock(Phrase, _, _) => throw new ParserException(
      s"Unexpected indent after '$Phrase'"
    )
  }
}
