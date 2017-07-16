package com.xantoria.frozen.parsing

import com.xantoria.frozen.parsing.block._
import com.xantoria.frozen.struct.{Package => PackageDef}

// FIXME: Rename
trait ClassParsing {
  def parseModule(s: String): PackageDef = parseModule(s.split("\n").toList)

  def parseModule(lines: Seq[String]): PackageDef = {
    implicit val indentType = IndentType("  ")  // FIXME

    val spaceStripped = lines filterNot { _.trim.isEmpty }
    val blocks = SplitBlocks(spaceStripped) map { Block(_) }

    val packageName = blocks.head match {
      case p: Package => p.name
      case b: Block => throw new ParserException(
        s"Expected package definition, found '${b.rawResult.head}'"
      )
    }
    if (!blocks.last.isInstanceOf[EOF]) {
      throw new ParserException(s"Expected '${EOF.Phrase}', found EOF")
    }

    val rawContent = blocks.map { _.rawResult }.flatten
    new PackageDef(packageName, rawContent)
  }
}
