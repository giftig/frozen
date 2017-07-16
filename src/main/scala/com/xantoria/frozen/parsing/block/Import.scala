package com.xantoria.frozen.parsing.block

import com.xantoria.frozen.parsing.ParserException

class Import(name: String) extends Block {
  val children: Seq[Block] = Nil
  val rawResult: Seq[String] = Seq(
    if (name contains ".") {
      val splitImport = name.split("\\.")
      val initial = splitImport.dropRight(1).mkString(".")
      val imported = splitImport.last
      s"from $initial import $imported"
    } else {
      s"import $name"
    }
  )
}

object Import {
  final val Pattern = """^([A-Za-z0-9_\.]+) is an open door!$""".r

  def apply: PartialFunction[UnparsedBlock, Import] = {
    case UnparsedBlock(Pattern(name), Nil, _) => new Import(name)
    case UnparsedBlock(Pattern(name), _, _) => throw new ParserException(
      s"Unexpected indent after import for '$name'"
    )
  }
}
