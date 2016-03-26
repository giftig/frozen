package com.xantoria.frozen

import java.io.File
import scala.io.Source

import com.xantoria.frozen.parsing.Parser
import com.xantoria.frozen.parsing.ParserException

object Main {
  private val parser = new Parser

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("No input files")
      System.exit(1)
    }

    val results = args map { arg: String => parseFile(new File(arg)) }
    val numErrors = results.filter { b: Boolean => !b }.size

    if (numErrors != 0) {
      println(s"Found errors in $numErrors files")
    }
  }

  def parseFile(f: File): Boolean = {
    val data = Source.fromFile(f).getLines.mkString("\n")
    try {
      parser.parse(data).write()
      true
    } catch {
      case e: ParserException => {
        println(s"\033[31m${e.getMessage}\033[0m")
        false
      }
    }
  }
}
