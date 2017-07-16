package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

abstract class Block {
  val children: Seq[Block]
  val rawResult: Seq[String]
}
case class UnparsedBlock(head: String, body: Seq[String], indentType: IndentType)

object Block {
  def apply(block: UnparsedBlock)(implicit indentType: IndentType): Block = {
    val fallback: PartialFunction[UnparsedBlock, Block] = {
      case block: UnparsedBlock => throw new ParserException(
        s"Unexpected symbol near ${block.head}"
      )
    }

    val func = {
      Package.apply orElse
      Import.apply orElse
      Class.apply orElse
      Method.apply orElse
      If.apply orElse
      Else.apply orElse
      Raw.apply orElse
      EOF.apply orElse
      fallback
    }

    func(block)
  }
  def apply(head: String, tail: Seq[String])(implicit indentType: IndentType): Block = Block(
    UnparsedBlock(head, tail, indentType)
  )
  /*
  case class While(c: List[String]) extends ParsedBlock(c)
  case class Statement(c: String) extends ParsedBlock(Nil)

  final val LOOP_PATTERN = """^while \((.+)\) olaf$""".r
  final val BREAK_PHRASE = "let it go, let it go"
  final val CONTINUE_PHRASE = "let the storm rage on"
  final val RETURN_PHRASE = "okay, bye..."
  */
}

object UnparsedBlock {
  def apply(content: Seq[String])(implicit indentType: IndentType): UnparsedBlock = UnparsedBlock(
    content.headOption getOrElse { throw new ParserException("Empty block") },
    ShiftIndent(content.tail, -1, indentType),
    indentType
  )
}
