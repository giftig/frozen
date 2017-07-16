package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

object ShiftIndent {
  def apply(code: String, level: Int, indentType: IndentType): String = ShiftIndent(
    code.split("\n").toList, level, indentType
  ).mkString("\n")

  def apply(lines: Seq[String], level: Int, indentType: IndentType): Seq[String] = {
    lines map {
      line: String => {
        val prefix = indentType.value * math.abs(level)
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
