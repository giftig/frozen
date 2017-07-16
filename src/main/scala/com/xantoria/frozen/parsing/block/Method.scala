package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

class Method(
  name: String,
  args: Seq[String],
  contents: Seq[UnparsedBlock],
  indentType: IndentType
) extends Block {
  // TODO: Some blocks aren't valid here. Imports, package?
  val children: Seq[Block] = contents map { Block(_)(indentType) }

  val rawResult: Seq[String] = {
    val head = s"def $name(${args.mkString(", ")}):"
    val body = children.map { _.rawResult }.flatten
    head +: ShiftIndent(body, 1, IndentType.default)
  }
}

object Method {
  final val Pattern = """^or ride an? ([A-Za-z_][A-Za-z0-9_]*) around the halls \((.+)\)\?$""".r

  def apply: PartialFunction[UnparsedBlock, Method] = {
    case UnparsedBlock(Pattern(name, args), tail, indentType) => new Method(
      name,
      parseArgs(args),
      SplitBlocks(tail)(indentType),
      indentType
    )
  }

  def parseArgs(raw: String): Seq[String] = raw.split(",") map { _.trim }

}
