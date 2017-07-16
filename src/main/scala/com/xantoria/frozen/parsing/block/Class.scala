package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

class Class(name: String, contents: Seq[UnparsedBlock], indentType: IndentType) extends Block {
  val children: Seq[Block] = {
    val blocks = contents map { Block(_)(indentType) }
    blocks foreach {
      case _: Method => ()
      case other => throw new ParserException(s"Expected a method definition but found $other")
    }
    blocks
  }
  val rawResult: Seq[String] = {
    val head = s"class $name(object):"
    val body = children.map { _.rawResult }.flatten
    head +: ShiftIndent(body, 1, IndentType.default)
  }
}

object Class {
  final val Pattern = """^do you wanna build an? ([A-Za-z_][A-Za-z0-9_]*)\?$""".r

  def apply: PartialFunction[UnparsedBlock, Class] = {
    case UnparsedBlock(Pattern(name), tail, indentType) => new Class(
      name,
      SplitBlocks(tail)(indentType),
      indentType
    )
  }
}
