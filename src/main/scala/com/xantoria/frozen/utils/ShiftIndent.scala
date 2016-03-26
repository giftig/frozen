package com.xantoria.frozen.utils

import com.xantoria.frozen.parsing.ParserException

object ShiftIndent {
  def apply(code: String, level: Int, indentType: String): String = ShiftIndent(
    code.split("\n").toList, level, indentType
  ).mkString("\n")

  def apply(lines: List[String], level: Int, indentType: String): List[String] = {
    lines map {
      line: String => {
        val prefix = indentType * math.abs(level)
        if (level < 0) {
          if (!line.startsWith(prefix)) {
            throw new ParserException(s"Underindented near '$line'")
          }
          line.substring(prefix.length)
        } else {
          prefix + line
        }
      }
    }
  }
}
