package com.xantoria.frozen.parsing.block

import scala.annotation.tailrec

/**
 * Find blocks within the given code and split them out
 *
 * A block is defined by a header line followed by a number of indented body lines
 */
object SplitBlocks {
  def apply(lines: Seq[String])(implicit indentType: IndentType): Seq[UnparsedBlock] = {
    @tailrec
    def iter(batch: Seq[String], acc: Seq[Seq[String]] = Nil): Seq[Seq[String]] = {
      val header :: remainder = batch

      val (firstBlockBody, otherBlocks) = remainder span {
        line: String => line.startsWith(indentType.value) || line.isEmpty
      }
      val firstBlock = header +: firstBlockBody

      otherBlocks match {
        case Nil => acc :+ firstBlock
        case content => iter(content, acc :+ firstBlock)
      }
    }

    iter(lines) map { UnparsedBlock(_) }
  }
}
