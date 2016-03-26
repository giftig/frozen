package com.xantoria.frozen.utils

import scala.annotation.tailrec

object SplitBlocks {
  def apply(code: String, indentType: String): List[String] = SplitBlocks(
    code.split("\n").toList, indentType
  ) map { _.mkString("\n") }

  def apply(lines: List[String], indentType: String): List[List[String]] = {
    @tailrec
    def iter(lines: List[String], acc: List[List[String]] = Nil): List[List[String]] = {
      val trimmedStart = lines.dropWhile { _.isEmpty }
      val header = trimmedStart.head
      val remainder = trimmedStart.tail

      val (firstBlockBody, otherBlocks) = remainder span {
        line: String => line.startsWith(indentType) || line.isEmpty
      }
      val firstBlock = header +: firstBlockBody

      otherBlocks match {
        case Nil => acc :+ firstBlock
        case content => iter(content, acc :+ firstBlock)
      }
    }

    iter(lines)
  }
}
