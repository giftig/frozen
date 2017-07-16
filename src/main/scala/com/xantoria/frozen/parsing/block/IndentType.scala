package com.xantoria.frozen.parsing.block

/**
 * Represents the type of indentation used in a block
 */
case class IndentType(value: String)

object IndentType {
  val default = IndentType("  ")
}
