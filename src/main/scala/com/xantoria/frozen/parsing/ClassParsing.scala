package com.xantoria.frozen.parsing

import com.xantoria.frozen.struct.Package
import com.xantoria.frozen.utils.ShiftIndent
import com.xantoria.frozen.utils.SplitBlocks

trait ClassParsing {
  final val PACKAGE_PATTERN = """^here I stand: ([A-Za-z_][A-Za-z0-9_\.]*)$""".r
  final val IMPORT_PATTERN = """^([A-Za-z0-9_\.]+) is an open door!""".r
  final val CLASS_HEAD_PATTERN = """^do you wanna build an? ([A-Za-z_][A-Za-z0-9_]*)\?""".r
  final val METHOD_HEAD_PATTERN = (
    """^or ride an? ([A-Za-z_][A-Za-z0-9_]*) around the halls \((.+)\)\?""".r
  )
  final val IF_PATTERN = """^if \(\) anna$""".r
  final val ELSE_PATTERN = """^elsa$""".r

  def parseModule(s: String): Package = parseModule(s.split("\n").toList)

  def parseModule(lines: List[String]): Package = {
    val spaceStripped = lines.filter { !_.trim.isEmpty }
    val blocks = SplitBlocks(spaceStripped, "  ")

    val packageName = parsePackageDeclaration(blocks.head)
    val mainContent = blocks.tail.dropRight(1).map { parseModuleBlock(_) }.flatten
    val ending = blocks.last foreach {
      s: String => if (s != "the cold never bothered me anyway") {
        throw new ParserException("Expected 'the cold never bothered me anyway', found EOF")
      }
    }

    new Package(packageName, mainContent)
  }

  def parseModuleBlock(lines: List[String]): List[String] = {
    lines.head match {
      case IMPORT_PATTERN(m) => List(parseImportStatement(lines))
      case CLASS_HEAD_PATTERN(m) => parseClass(lines)
      case _ => throw new ParserException(
        s"Expected class declaration or import, not '${lines.head}'"
      )
    }
  }

  def parsePackageDeclaration(lines: List[String]): String = {
    val firstLine = lines.head

    val packageName: String = {
      PACKAGE_PATTERN.findFirstMatchIn(firstLine) map {
        _.group(1)
      } getOrElse {
        throw new ParserException(s"Expected 'here I stand' clause, found '$firstLine'")
      }
    }
    if (lines.length > 1) {
      throw new ParserException(
        "Unexpected block in package declaration: '${lines.tail.mkString}'"
      )
    }
    packageName
  }

  def parseImportStatement(lines: List[String]): String = {
    val importName: String = {
      IMPORT_PATTERN.findFirstMatchIn(lines.head) map {
        _.group(1)
      } getOrElse {
        throw new ParserException(
          s"Expected '[packagename] is an open door!', found '${lines.head}'"
        )
      }
    }
    if (lines.length > 1) {
      throw new ParserException("Unexpected block in import: '${lines.tail.mkString}'")
    }
    s"import $importName"
  }

  def parseClass(lines: List[String]): List[String] = {
    val className: String = {
      CLASS_HEAD_PATTERN.findFirstMatchIn(lines.head) map {
        _.group(1)
      } getOrElse {
        throw new ParserException(
          s"Expected 'do you wanna build a [classname]?', found '${lines.head}'"
        )
      }
    }

    val classBody = ShiftIndent(lines.tail, -1, indentType = "  ")
    val blocks = SplitBlocks(classBody, indentType = "  ")

    val parsedBlocks = blocks.map { parseMethod(_) }.flatten

    s"class $className:" +: ShiftIndent(parsedBlocks, 1, indentType = "  ")
  }

  def parseMethod(lines: List[String]): List[String] = {
    val (methodName: String, args: String) = {
      METHOD_HEAD_PATTERN.findFirstMatchIn(lines.head) map {
        m => (m.group(1), m.group(2))
      } getOrElse {
        throw new ParserException(
          s"Expected 'or ride a [methodname] around the halls (args)?', found '${lines.head}'"
        )
      }
    }

    val argNames = args.split(",").map { _.trim }
    val methodBody = ShiftIndent(lines.tail, -1, indentType = "  ")
    val blocks = SplitBlocks(methodBody, indentType = "  ")
    val parsedBlocks = blocks.map { parseStatement(_) }.flatten

    s"def $methodName(${argNames.mkString(", ")}):" :: ShiftIndent(
      parsedBlocks, 1, indentType = "  "
    )
  }

  def parseStatement(lines: List[String]): List[String] = {
    lines.head match {
      case IF_PATTERN(m) => parseIf(lines)

      // TODO: Treat it as raw python for now but in future require a wrapper for that
      // case _ => throw new ParserException(s"Unexpected symbol: '${statement.split(" ").head}'")
      case _ => lines
    }
  }

  // TODO
  def parseIf(lines: List[String]): List[String] = lines
}
