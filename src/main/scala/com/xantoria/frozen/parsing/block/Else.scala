package com.xantoria.frozen.parsing.block

class Else(body: Seq[UnparsedBlock], indentType: IndentType) extends Block {
  val children: Seq[Block] = body map { Block(_)(indentType) }
  val rawResult: Seq[String] = {
    val body = children.map { _.rawResult }.flatten
    "else:" +: ShiftIndent(body, 1, IndentType.default)
  }
}

object Else {
  def apply: PartialFunction[UnparsedBlock, Else] = {
    case UnparsedBlock("elsa", tail, indentType) => new Else(
      SplitBlocks(tail)(indentType),
      indentType
    )
  }
}
