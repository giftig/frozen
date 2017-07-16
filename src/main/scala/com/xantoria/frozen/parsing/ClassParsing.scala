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

    val mainMethod = identifyMain(blocks) map {
      case (cls, method) => new MainInvocation(cls, method)
    }

    val adjustedContent = blocks ++ mainMethod

    // TODO: Do some more sanity checking. eg. look for else without if, package statements or
    // EOFs inside other blocks, etc.

    val rawContent = adjustedContent.map { _.rawResult }.flatten
    new PackageDef(packageName, rawContent)
  }

  def identifyMain(content: Seq[Block]): Option[(String, String)] = {
    val result: Seq[(String, String)] = content.collect {
      case cls: Class => cls.children map {
        case method: Method if method.name == "main" => (cls.name, method.name)
      }
    }.flatten

    result match {
      case result :: Nil => Some(result)
      case _ => None
    }
  }
}
