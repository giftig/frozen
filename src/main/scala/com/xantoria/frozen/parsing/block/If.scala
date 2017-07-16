package com.xantoria.frozen.parsing.block

class If(cond: String, body: Seq[UnparsedBlock], indentType: IndentType) extends Block {
  val children: Seq[Block] = body map { Block(_)(indentType) }
  val rawResult: Seq[String] = {
    val head = s"if $cond:"
    val body = children.map { _.rawResult }.flatten
    head +: ShiftIndent(body, 1, IndentType.default)
  }
}

object If {
  final val Pattern = """^if \((.+)\) anna$""".r

  def apply: PartialFunction[UnparsedBlock, If] = {
    case UnparsedBlock(Pattern(cond), tail, indentType) => new If(
      cond,
      SplitBlocks(tail)(indentType),
      indentType
    )
  }
}
